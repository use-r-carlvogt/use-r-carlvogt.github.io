# Author : Elise Tancoigne - elise.tancoigne@unige.ch
# License: CC-by 4.0 (https://creativecommons.org/licenses/by/4.0/)
# February 7th, 2017

# this R script is an introduction to the packages rtweet and graphTweets 
# I wrote it for a short presentation at the University of Geneva's R lunchs.
# The associated slides are available on the website: http://use-r-carlvogt.github.io/prochains-lunchs/

library(rtweet)

##################################################
###### CREATE CONNECTION WITH TWITTER API

# get your consumerKey and consumerSecret accesses. See how to do it on:
vignette("auth")
# or read https://mkearney.github.io/rtweet/articles/auth.html

consumerKey <- "the_consumer_key_provided_by_twitter"
consumerSecret <- "the_consumer_secret_provided_by_twitter"
token <- create_token(app = "your_application_name", consumerKey, consumerSecret)


##################################################
###### PLAY WITH THE FUNCTIONS

# check rate limitations
rate_limit(token, query="users/search")
rate_limit(token, query="search/tweets")
# a list of twitter functions can be found at: https://dev.twitter.com/rest/reference

# get 200 last tweets from @unige_ise
get_timeline("@unige_ise")
View(get_timeline("@unige_ise"))
rate_limit(token, query = "statuses/user_timeline")

ise_timeline <- get_timeline("@unige_ise")

# plot their frequencies
ts_plot(ise_timeline, by = "days")
ts_plot(ise_timeline, filter = "Rhône", by = "months")
ts_plot(ise_timeline, filter = "@UNIGEnews", by = "months")

# get followers & friends
ise_followers <- get_followers("@unige_ise")
View(ise_followers)

ise_friends <- get_friends("@unige_ise")
View(ise_friends)


# retrieve data for a set of users
ise_foll_data <- lookup_users(ise_followers)
rate_limit("users/lookup", token = token)


# search tweets through keywords
?search_tweets
a <- search_tweets("honey almonds", n=18000)
b <- search_tweets("honey AND almonds", n=18000)
c <- search_tweets("honey OR almonds", n=18000)
d <- search_tweets('"honey almonds"', n=18000)
View(d)

# search users through keywords
?search_users
a <- search_users("saucisson sec", n=18000)
b <- search_users("saucisson OR sec", n=18000)
c <- search_users('"saucisson sec"', n=18000)
View(c)

# stream API
f <- stream_tweets("@nytimes")


##################################################
###### BUILD THE NETWORK OF CITATIONS WITH graphTweets
# (who cites who within a tweet)

library(graphTweets)

edges <- getEdges(data = ise_timeline, tweets = "text", source = "screen_name")
View(edges)
sort(table(edges$target), decreasing = T)[1:10]


library(igraph)
# plot the graph
g <- graph.data.frame(edges, directed=TRUE)
plot(g)

# reduce the number of nodes displayed according to a chosen criteria
g <- graph.data.frame(edges[which(edges$retweet_count>2),], directed=TRUE)
plot(g)



