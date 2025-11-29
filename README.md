Explanation of Random Walker implementation.
# CORE IDEA:
Core idea behind Random Walk, Eigen Centrality, and Katz Centrality measures is to identify which one offers more suitable method for a recommender system. 
Although Similarity and Recommender systems were 2 separate RQs, they are interdependent. Having said that, the following is my approach to Random Walker.
# 2 Types of Random Walker algorithm.
To explain the following algorithms, suppose we have a one-mode projection onto stories of our bi-partite graph of Stories and Tags:
### Here S1 and S2 are connected by T1, T2, T3, T4, T5, whereas S1 and S3 are only connected by T6


(T1) (T2) (T3) (T4) (T5)                          (T6)              
 \   \    |     /    /                            / \
   \  \   |    /    /                           (S1) (S2) 
     \ \  |   /    /
         (S1) 

(T1) (T2) (T3) (T4) (T5)                          
 \   \    |     /    /                            
   \  \   |    /    /                           
     \ \  |   /    /
         (S2) 

```

First, there is a regular Random Walker, which uses PageRank. It starts off at a user-specified Story Node S1 and has a parameter Damping, which is a probability that it 
continues to walk to the next node. 1-Damping is the probability that our Walker will return to node S1, which we specified. In real-world terms, you may think of 
returning to starting Node S1 as refreshing the YouTube feed, or hitting the Google icon, which returns to the  homepage with a search bar. In case Walker continues
to explore other nodes, they randomly select 1 out of 6 Tags at Story 1. Let's say Walker selects Tag 3, then, Walker can only choose S2 to walk to. 
If we run the PageRank, it gives us a score of Nodes that are STRUCTURALLY most similar to our starting node S1. It does so for the following reasons:
S1 and S2 share 5 Tags, whereas S1 and S3 share only 1 Tag. Since we can't tell which Tags are INFORMATIVE/DESCRIPTIVE, and which are NOT DESCRIPTIVE. We weigh them equally.
In other words, if Harry Potter Story was tagged by 'Mystery', 'Hogwarts', 'Magic', 'Rowling', 'UK', 'Slytherin', without running any fancy NLP, we can't tell
if 'Slytherin' is a better descriptor than 'Magic'. Therefore, if we were to recommend another story to a user currently reading Harry Potter, we may explore 'Magic',
and try 'Loran's Smile' or jump to 'Rowling' and recommend 'The Kuckoo's Calling', which is one of her other writings. 
Having said that, if we run the PageRank 100 times, it will return the number of times a certain story node was chosen/all walks, giving us the probability.
In our simple graph example, the probability that the user will walk from S1 to S2 is (0.85) * (5/6), where 0.85 is the probability that the user will continue exploring.
From S1 to S3, the probability is (0.85) * (1/6), much lower than S2. As we noted above, S1 and S2 are structurally similar due to their sharing 5 Tags, therefore, 
The score that PageRank returns would be equivalent to the number of times/all times that a walker visited a certain node, where the probability will be naturally higher
When the visited node is structurally similar, ie shares a lot of Tags with the starting node. 
# Pros and Cons:
Pros of this method are simplicity, lack of vagueness, and clear metrics. We get the initial node from the user, and we ask the user to pick what they like.
Cons of this method are that we require initial input from the user, and it underperforms in complex graphs. 



# The Second Revision of Random Walker.
We call it the Sticky Random Walker, where it imitates the real-world behaviour of users of Netflix, YouTube, Kindle, etc. 
It adds a parameter called Drift Resistance, story popularity and pre-computed Louvain clusters.

1) Here, when the walker is on a Tag Node, it weighs edges to Stories by their popularity, which is Log(kudos+comments+1)+1. Therefore, we assume the wisdom of the crowd,
and recommend stories that are liked by others.

2) Let's assume the user is currently reading Harry Potter, and they choose a Tag 'Magic', which leads them to multiple stories associated with 'Magic'. Let's say the user selects
'Magic Tricks', and let's assume that 'Magic' is the only Tag that connects a story 'Magic Tricks' with 'Harry Potter'. Because of a weak connection, the Louvain algorithm
puts 'Harry Potter' and 'Magic Tricks' into 2 separate communities. Here we assume that the user made a sane choice by jumping into another community; therefore,
if Random Walker decides to 'reset', which happens 1 - Damping times, the  user will no longer reset to the Starting Node, but they will reset to 1st story in their new community.
In this example, if a user were to reset after reading a few stories in 'Magic', they would reset to 'Magic Tricks'.

3) Drift Resistance. This was an ADHD special, where attention might drift away very fast, and it is a controllable parameter. What it defines is the probability that
the user will jump to a weakly related, and far community. For example, if a user were on a story about 'Michelin Pilot Sport 4S' which is a high performance summer tire,
and Michelin happens to be the same company that also rates restaurants, Drift Resistance controls how many times our user would jump from a tire review into a creme brulee recipe.
The main purpose is to control how targeted/random our user wants the recommendations to be.



