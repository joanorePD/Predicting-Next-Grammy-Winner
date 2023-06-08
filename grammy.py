import requests
from bs4 import BeautifulSoup

grammys_raw = requests.get('https://www.grammy.com/awards/61st-annual-grammy-awards-2018').text

grammys_soup = BeautifulSoup(grammys_raw, 'html5lib')

grammys_groups = grammys_soup.find_all('div',{'id':'__next'})
grammys_groups = grammys_groups[0].find_all('div',{'class':'flex flex-col md-xl:justify-center min-h-screen'})
grammys_groups = grammys_groups[0].find_all('main',{'class':'flex flex-col items-center justify-center w-full flex-1'})
#grammys_groups = grammys_groups[0].find_all('div')
grammys_groups = grammys_groups[0].find_all('section',{'class':'flex flex-col w-full max-w-335px md-xl:max-w-full md-xl:px-8 items-center justify-center'})
#grammys_groups = grammys_groups[0].find_all('div',{'class':''})


print(len(grammys_groups))
print(grammys_groups)