# RM_CSCI460
Roderic Moreno Data Capstone Project

# Week 1

This week was used to set up my IDE to link it to this repository, as well as create the README file which will serve as this blog.

5/21/26

I also began to browse the Riot Games API in order to come up with a problem that can be solved using data pulled from that source.


# Week 2

5/27/26

This week was spent scraping match data in order to finalize a project scope and trying to come up with a robust plan.


# Project Background

League of Legends game history and stuff--I'll figure out a background; but in essence, there are several "breakpoints" in each game, and with 172 characters in the game, there are bound to be some which excel at earlier breakpoints, and some whose power doesn't spike until much later. I hope to have a data-driven analysis which can accurately describe each champion.


# Project Description

This project aims to create a tool where someone can simulate a draft phase of a League of Legends match by inputting champions into red and blue side. It uses data from top ranked games from the most recent patch, patch 26.10, to develop a model that predicts which side has the advantage to take the three early game objectives, and provide insight into how to play with a team composed of those 5 champions.


# 5/29/26

Today, I created functions in order to streamline the API requests and begin building a dataset. The next steps to this are to begin pulling data from the matches, recoding data to fit the current theory that I have, and crafting a model which can be used to predict the 3 or 4 target variables. 

Important links:
 https://developer.riotgames.com/apis 
This is the webpage accessing the Riot games APIs. The APIs used for this are MATCH-V5 and ACCOUNT-V1, which contain all of the data necessary for this project. 


 https://drafting.gg/draft
This is a "mock draft" website, wherein you draft champions by putting them into the Blue and Red teams. My project will have a tool similar to this, but inputting the champions will give the predictions.

 https://dignitas.gg/articles/a-guide-to-help-you-take-objectives-in-league-of-legends
This website gives a brief overview of neutral objectives in League of Legends.
