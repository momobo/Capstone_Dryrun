

# ---- check manually
trigram <- "i am tired"

probTrigram("i am tired")


#x4 <- "a b c"
corp <- readLines(trainfile, encoding="UTF-8")
?grep
grep("a case of beer", corp) # 2      bigram   0.0001518145
grep("a case of soda", corp) # 0
grep("a case of cheese", corp) # 0    unigram 0.0001777146
grep("a case of pretzels", corp) # 0

grep("would mean the universe", corp)     # bigram 0.001751835
grep("would mean the world", corp) # 1    # bigram 0.01713912
grep("would mean the most", corp)         # bigram 0.01212972
grep("would mean the best", corp)         # bigram 0.0130694

quiz("me the", c("happiest", "smelliest", "bluest", "saddest")) # happiest
grep("make me the happiest", corp) #1
grep("make me the smelliest", corp)
grep("make me the bluest", corp)
grep("make me the saddest", corp)


quiz("but the", c("crowd", "referees", "players", "defense")) # crowd
grep("struggling but the crowd", corp)
grep("struggling but the referees", corp)
grep("struggling but the players", corp)
grep("struggling but the defense", corp)
grep("but the crowd", corp) #6
grep("but the referees", corp)
grep("but the players", corp) #1
grep("but the defense", corp)


quiz("at the", c("mall", "grocery", "movies", "beach")) # mall/beach
grep("date at the mall", corp)
grep("date at the grocery", corp)
grep("date at the movies", corp)
grep("date at the beach", corp)
grep("at the mall", corp) #36
grep("at the grocery", corp) #94
grep("at the movies", corp) #8
grep("at the beach", corp) #111

quiz("on my", c("motorcycle", "horse", "way", "phone")) # way
grep("be on my motorcycle", corp)
grep("be on my horse", corp)
grep("be on my way", corp) #6
grep("be on my phone", corp)

quiz("quite some", c("time", "thing", "years", "weeks")) # time
grep("in quite some time", corp)
grep("in quite some thing", corp)
grep("in quite some years", corp)
grep("in quite some weeks", corp)
grep("quite some time", corp) #154
grep("quite some thing", corp) #1
grep("quite some years", corp) #1
grep("quite some weeks", corp)

# candidate to KN
quiz("his little", c("toes", "eyes", "fingers", "ears")) # eyes
grep("with his little toes", corp)
grep("with his little eyes", corp)
grep("with his little fingers", corp)
grep("with his little ears", corp)
grep("his little toes", corp)
grep("his little eyes", corp) #3
grep("his little fingers", corp) #3
grep("his little ears", corp)
grep("little eyes", corp)    # 15
grep("little fingers", corp) # 16



quiz("during the", c("worse", "sad", "bad", "hard")) # worse?? wrong
grep("faith during the worse", corp)
grep("faith during the sad", corp)
grep("faith during the bad", corp)
grep("faith during the hard", corp)
grep("during the worse", corp)
grep("during the sad", corp)
grep("during the bad", corp)
grep("during the hard", corp) #1


quiz("must be", c("asleep", "insane", "callous", "insensitive")) # asleep? wrong
grep("you must be asleep", corp)
grep("you must be insane", corp)
grep("you must be callous", corp)
grep("you must be insensitive", corp)
grep("must be asleep", corp)
grep("must be insane", corp) #1
grep("must be callous", corp)  
grep("must be insensitive", corp)




head()
head(ntddf4, 100)
# need complete ntddfx and functions brokeInPhases, vecToCorpus
rm(x4)
teststring2("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
# ???
teststring2("You're the reason why I smile everyday. Can you follow me please? It would mean the")
# world
teststring2("Hey sunshine, can you follow me and make me the")
# ???
teststring2("Very early observations on the Bills game: Offense still struggling but the")
teststring2("Go on a romantic date at the")
teststring2("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
teststring2("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
teststring2("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
teststring2("Be grateful for the good times and keep the faith during the")
teststring2("If this isn't the cutest thing you've ever seen, then you must be")

teststring2("case of soda")
teststring2("I was in")

head(ntddf4, 2000)
