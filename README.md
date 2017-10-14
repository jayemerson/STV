# STV
#
Implementations of the Single Transferable Vote (STV) counting 
system. By default, it uses the Cambridge method for surplus allocation
and Droop method for quota calculation.  Fractional surplus allocation
and the Hare quota are available as options.

<b>Current package build status</b> via use of
[Travis CI](https://travis-ci.org/jayemerson/STV):
<a href="https://travis-ci.org/jayemerson/STV"><img src="https://travis-ci.org/jayemerson/STV.svg?branch=master"></a>

# More Information on STV

See a page from the <a href="https://www.electoral-reform.org.uk/voting-systems/types-of-voting-system/single-transferable-vote/">Electoral Reform Society</a>, or
of course <a href="https://en.wikipedia.org/wiki/Single_transferable_vote">Wikipedia</a>.
A second Wikipedia page describes the 
<a href="https://en.wikipedia.org/wiki/History_and_use_of_the_single_transferable_vote">history
and use of the STV system.</a>
There is also a <a href="https://www.youtube.com/watch?v=l8XOZJkozfI">cute video
cartoon</a> on YouTube that you might enjoy.

# Getting Started

To load the package in R directly from github, you may use the following commands:

---
    > library(devtools)
    > install_github('jayemerson/STV')
    > library(STV)
---

# Feedback

Although there are many ways of counting single transferable votes, this package supports only two quota methods and two surplus reallocation methods. We welcome collaboration as we expand the vote count method options.

# Jay's Github notes (because I can never remember...)

After creating the public repository on GitHib, essentially empty
with only `README.md`, I cloned to my laptop:

    git clone https://github.com/jayemerson/STV.git
    
I then moved the package contents into the local `STV` directory,
at which point the following workflow appears sufficient for my own
purposes:

---
    git status
    git commit -am "Short commit message here"
    git push origin
---

If I'm not working with anyone, I'm pretty much set.  I do work and then
push the changes, end-of-story.  But if someone submits a pull request
to the master on Github that I accept, I need to update my local copy
with this new contribution:

---
    git pull
---

At this point, I think that's it.  I'm not using any special branches
or anything (maybe I should, but I'm not).
