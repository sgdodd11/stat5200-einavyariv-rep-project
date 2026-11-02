"""Collect Scopus authors whose most frequent subject area is economics.

AUTHOR PERSPECTIVE (kept for reference; superseded by scopus_extract_docFilter.py)

What this script does:
  1. Runs a Scopus Author Search for SUBJAREA(ECON), narrowed to authors whose
     affiliation mentions economics, business or finance (AUTHOR_QUERY).
  2. Pages through the results 25 authors at a time (one API request per page).
  3. Keeps only authors whose most frequent subject area is ECON, stopping at
     TARGET economists or MAX_PAGES requests, whichever comes first.
  4. Writes ID, name, affiliation, subject areas and document count to CSV.

Why we moved to the document + field perspective instead:
  - SUBJAREA(ECON) in Author Search matches anyone with at least one economics
    paper, so the pool is mostly people in other fields.
  - Scopus cannot filter authors on their *main* subject area; that check can
    only happen here, after each page has been downloaded.
  - Results come back sorted by document count, so the first pages are prolific
    medical and engineering researchers. A test run checked 1,000 authors over
    40 requests and found only 1 whose main area was ECON.
  - Searching documents instead lets Scopus filter to economics papers on its
    side, so nearly every author ID collected is a likely economist and far
    fewer requests are wasted.
  - The affiliation filter in step 1 was added afterwards to test whether a
    Scopus-side pre-filter makes the author perspective workable. It misses
    economists whose affiliation doesn't use these words (e.g. policy schools,
    central banks).
"""

from pathlib import Path
from urllib.parse import quote
from elsapy.elsclient import ElsClient
import csv
import json

# Locate files relative to this script
config_path = "../config.json"
output_path = Path(__file__).resolve().parent.parent / "data" / "econ_author_ids_test.csv"

TARGET = 10        # Number of economists to keep
PAGE_SIZE = 25     # Authors per API request
MAX_PAGES = 40     # Stop after this many requests even if TARGET isn't reached

# Load API credentials
with open(config_path) as con_file:
    config = json.load(con_file)

client = ElsClient(config["apikey"])
client.inst_token =  None

# Pre-filter on Scopus's side: any econ papers, plus an economics, business or
# finance affiliation
AUTHOR_QUERY = "SUBJAREA(ECON) AND (AFFIL(economics) OR AFFIL(business) OR AFFIL(finance))"
base_url = "https://api.elsevier.com/content/search/author?query=" + quote(AUTHOR_QUERY)


def subject_list(author):
    """Return the author's subject areas as a list, most frequent first."""
    # Usually a list, but a single subject area can come back as a dict
    subjects = author.get("subject-area") or []
    if isinstance(subjects, dict):
        subjects = [subjects]
    return sorted(subjects, key=lambda s: int(s.get("@frequency") or 0), reverse=True)


def is_economist(author):
    """True if ECON is the author's most frequent subject area."""
    subjects = subject_list(author)
    return bool(subjects) and subjects[0].get("@abbrev") == "ECON"


# Page through matching authors until TARGET economists are found
economists = {}
checked = 0
for page in range(MAX_PAGES):
    response = client.exec_request(f"{base_url}&start={page * PAGE_SIZE}&count={PAGE_SIZE}")
    results = response["search-results"]
    total = int(results["opensearch:totalResults"])

    for author in results.get("entry", []):
        identifier = author.get("dc:identifier")
        if not identifier:
            continue
        checked += 1
        if is_economist(author):
            economists[identifier.split(":")[-1]] = author
        if len(economists) >= TARGET:
            break

    print(f"Page {page + 1}: checked {checked} authors, kept {len(economists)}")
    if len(economists) >= TARGET or (page + 1) * PAGE_SIZE >= total:
        break

print("Total matching authors:", total)
print(f"Economists found: {len(economists)} of {checked} authors checked")

# Display the first five economists
for author_id, author in list(economists.items())[:5]:
    print(json.dumps(author, indent=2))
    print("Author ID:", author_id)

# Build one row per economist
fieldnames = [
    "scopus_author_id",
    "surname",
    "given_name",
    "affiliation",
    "subject_areas",
    "document_count",
]

rows = []
for author_id, author in economists.items():
    name = author.get("preferred-name") or {}
    # Missing for some authors
    affiliation = author.get("affiliation-current") or {}

    rows.append({
        "scopus_author_id": author_id,
        "surname": name.get("surname"),
        "given_name": name.get("given-name"),
        "affiliation": affiliation.get("affiliation-name"),
        # Broad areas, most frequent first; the same area can repeat, so dedupe
        "subject_areas": ";".join(dict.fromkeys(s["@abbrev"] for s in subject_list(author) if s.get("@abbrev"))),
        "document_count": author.get("document-count"),
    })

output_path.parent.mkdir(parents=True, exist_ok=True)

with output_path.open("w", newline="", encoding="utf-8") as out_file:
    writer = csv.DictWriter(out_file, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(rows)

print(f"Saved {len(rows)} authors to {output_path}")
