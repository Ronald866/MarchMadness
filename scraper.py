import urllib2
import csv
from bs4 import BeautifulSoup

def main():
    teams = get_teams()
    stats = get_stats_for_teams(teams)
    headers = stats[0].keys()
    with open("stats.csv", "w") as csv_file:
        writer = csv.DictWriter(csv_file, headers)
        writer.writeheader()
        for team in stats:
            writer.writerow(team)

def get_teams():
    stat_page = "https://www.sports-reference.com/cbb/friv/forecast.cgi"
    page = urllib2.urlopen(stat_page)
    soup = BeautifulSoup(page, "html.parser")
    table = soup.find("table", attrs={"id":"forecast"}).tbody
    teamRows = table.find_all("tr", class_=lambda x: x != "thead")
    teams = []
    for t in teamRows:
        teams.append(get_team_bracket_info(t))
    return teams

def get_team_bracket_info(teamRow):
    team = {}
    team["name"] = teamRow.find("a")["href"].split("/")[3]
    team["region"] = teamRow.find("td", attrs={"data-stat":"region"}).text.strip()
    team["seed"] = teamRow.find("td", attrs={"data-stat":"seed"}).text.strip()
    return team

def get_stats_for_teams(teams):
    team_stats = []
    for team in teams:
        tbody_soup = get_tbody_soup(team["name"])
        stats = get_stats(tbody_soup)
        team.update(stats)
        team_stats.append(team)
    return team_stats

def get_tbody_soup(team):
    try:
        stat_page = "https://www.sports-reference.com/cbb/schools/{}/2018.html".format(team)
        page = urllib2.urlopen(stat_page)
        soup = BeautifulSoup(page, "html.parser")
        return soup.find("table", attrs={"id":"team_stats"}).tbody
    except urllib2.HTTPError, e:
        print team + " could not be found"

def get_stats(tbody):
    stats = {}
    stats["gs"] = get_stat(tbody, "g")
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

