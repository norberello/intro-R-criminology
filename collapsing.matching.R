## For my specific issue (although to be honest, I'm not sure how
## generalizable it is to other students), I'm having challenges
## collapsing a dataset and matching the collapsed groups over time.
## My dataset deals with individuals who belong to different groups
## over time. Not only do individuals come and go across different
## time periods, but also the group they belongs to changes over time.

## I'm trying to match groups over time (ie is it the same, or a
## different group) based on the ratio of members who are present
## within the group across both time periods (I use a fairly
## straightforward formula): Where the numerator is the number of
## actors who are present in the group across both time t and time t+1
## and the denominator is the total number of actors in both groups.

## For example if in time 1 the group consisted of 10 offenders and in
## time 2 the group consisted of 15 offenders, and 5 of the offenders
## were present across both time periods, this would create a ratio of
## .25%

## The problem is is that individuals will split off into different
## groups, and I only want a group to be matched with the previous
## time period if it has the highest ratio of actor overlap.

## For example, in the attached excel sheet, group 3 consists of two
## actors in t1; however, in t2 both actors move into different
## groups. One actor moves into a different group with 3 new actors
## (ratio: .17) ; and the other actor is in a new group with 1 new
## actor (ratio: .33). I want to be able for the data to be collapsed
## and recognize that only the ratio .33 is the same group over time
## (highest actor overlap) and that the group with the ratio of .17
## actually becomes a new group (group 4).

library(data.table)
actors.groups <- fread("GDR Collapsing and Matching Dataset Issue.csv")
