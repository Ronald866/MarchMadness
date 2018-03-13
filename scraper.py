import urllib2
import csv
from bs4 import BeautifulSoup

def main():
    stats = get_stats_for_teams(["buffalo"])
    with open("stats.csv", "w") as csv_file:
        writer = csv.DictWriter(csv_file, ["team","ppg", "opg", "fg_pct", "op_fg_pct", "tov", "op_tov"])
        writer.writeheader()
        for team in stats:
            writer.writerow(team)

def get_stats_for_teams(teams):
    team_stats = []
    for team in teams:
        tbody_soup = get_tbody_soup(team)
        stats = get_stats(tbody_soup)
        stats["team"] = team
        team_stats.append(stats)
    return team_stats

def get_tbody_soup(team):
    stat_page = "https://www.sports-reference.com/cbb/schools/{}/2018.html".format(team)
    page = urllib2.urlopen(stat_page)
    soup = BeautifulSoup(page, "html.parser")
    return soup.find("table", attrs={"id":"team_stats"}).tbody

def get_stats(tbody):
    print(tbody)
    stats = {}
    stats["ppg"] = get_stat(tbody, "pts_per_g")
    stats["opg"] = get_stat(tbody, "pts_per_g", True)
    stats["fg_pct"] = get_stat(tbody, "fg_pct")
    stats["op_fg_pct"] = get_stat(tbody, "fg_pct", True)
    stats["tov"] = get_stat(tbody, "tov")
    stats["op_tov"] = get_stat(tbody, "tov", True)
    return stats
        
def get_stat(tbody, stat, is_opp = False):
    if is_opp:
        tag = "opp_{}"
    else:
        tag = "{}"
    fm_tag = tag.format(stat)
    return tbody.find("td", attrs={"data-stat": fm_tag}).text.strip().decode("utf-8", "ignore")

