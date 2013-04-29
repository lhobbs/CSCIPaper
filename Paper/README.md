
Introduction
============

Successful programming languages tend to evolve over time, especially in the context of their community. Some languages use standards committees to represent this community; others use more informal processes. Either way, any potential evolution is frequently encapsulated into a change proposal, which the community can evaluate, discuss, and further enhance. Frequently the change proposal is rejected outright, but sometimes it may live on in some other way, or even influence another language.


Choosing a topic
=================

In this assignment you will pick a language you know well, or at least have some significant exposure to the domain it is often used in. However, to constrain the language choices for this assignment, your chosen language must be in the [Tiobe top 50][] languages list. However some languages in that list will not be good choices. Consult the resources section, updating as necessary.

Once picked, find a reasonably substantial change proposal for that language and choose that as your paper topic. This change proposal may be currently in a proposed state, or it may now be rejected, accepted, or implemented. You may find it most straightforward to write about accepted proposals that are now implemented. However, rejected proposals may be of especial interest if the language later returned to that proposed idea, but in some other way.

Change proposals will be in the form of a PEP, SIP, JEP, JSR, or similar document. You may want consider subsequent proposals that cite the proposal as part of the development of your theme. See below for resources.

And then what? You need to carefully consider the change proposal in terms of documenting its proposed impact to the language and its use.


Writing your paper
==================

Divide into teams of three, using the partner selection tool in Piazza as necessary. Collaboration with your team will constitute a large portion of your grade for this paper.

Your paper on this change proposal should exhibit the following characteristics:

* Apply the terminology and concepts we have used throughout the course. If language feature Baz supports higher-order functions, and takes advantage of nested scopes and currying, state so in such terms and concepts. If feature Bar provides syntax that makes certain common tasks easier or less error prone in the language, perhaps describe the syntax in the context of allowed grammar productions.

* Provide supporting code examples, diagrams, and other evidence. Such examples can be original to you or can come from the change proposal and any corresponding discussion.

* Cite supporting evidence using primarily primary sources from the community of the language being investigated.

* As usual, academic standards of plagiarism and attribution do apply.

When citing evidence, you should look for authority over form. For this paper, blog posts and their comments, emails in mailing lists, and Q&A in StackOverflow-type sites are valid primary sources, even if informal, along with articles and books. What matters here is the content of the discussion and where it went. Consider carefully the author of the source, and its centrality, in the conversation. In addition, do filter out noise and irrelevance. Careful selection and presentation are important parts of what we will evaluate.

Papers must be written in [Markdown][], specifically using the [GitHub dialect][]. However, you are allowed to embed Latex for mathematical notation, as you see fit, using the following convention from Pandoc: `$ <latex goes here> $`. (GitHub used to render such notation with MathJax, and it's a major missing feature for us at this time.)

Papers must be between 1000 and 1500 words, excluding code fragments, long quotations (introduced via Markdown's `>`), and other metadata, such as images or citations links. We will provide a tool to test the word count compliance of your paper. Extra credit is available for writing/enhancing this tool. Incidentally, this assignment itself is approximately 1100 words in length.

Use Markdown fences (`~~~~~`) to demarcate code examples, except for very short fragments (less than a few lines). Introduce long quotes with `>`. Use endnote style links: use `[text][optional-id-if-different]` in the text, followed by an endnote with corresponding link at the end.

Lastly, the work on this paper should be used to support your team presentation ("lightning talk") to the class; more details on that will follow.


Grading rubric
==============

30% of your grade is a team grade. This evaluation will be based on reviewing the *written* collaboration on your assignments with other members on your team by using Github's facilities for peer review.

The remaining 70% of the grade will be evaluated with respect to the expected characteristics. In particular, do not use personal opinion, unless it can be supported by evidence. Don't use fuzzy language. This means to not write sentences like the following: "I think feature Baz in language Foo is amazing because it's so helpful and consequently my code will never have errors again!"


Important dates
===============

* WIP pull request on April 3 at 6p. In the WIP, describe the topic of your paper, list your team members, expected sources, code examples, and status of your work. You should have your TA approve your plan for the paper at this time, or earlier.
* Rough draft due on April 22 at 6p, with corresponding WIP tasks.
* Final draft due on May 2 at 6p. Note that this is also the final date that any work can be turned in for the course. In the accompanying WIP, make sure you communicate any information to your TA re the state of your paper.

Your grade in part will be affected by adherence to the dates above.


Resources
=========

Below are the resources you should start in your investigation:

* Python uses [PEPs][] (Python Enhancement Proposals) for language proposals. Two mailing lists are typically used, [python-ideas][] and [python-dev][] to discuss PEPs and their implementation. In addition, you may find the "What's new in Python x.y" useful in understanding the scope of changes; you may want to look at series of these "What's new" summations.
* Scala use SIPs (Scala improvement processes). FIXME.
* Java uses two community processes, JEPs and JSRs. FIXME.

FIXME (keep for now) other languages should be added above, with links; see below for extra credit.


Extra credit opportunities
==========================

We welcome your contributions to better defining this assignment. Extra credit, in proportion to the work submitted, will be awarded for merged pull requests, per our usual policy. Here are some potential ideas:

* Extending the resources section, either for other languages or with helpful other ideas.
* Finding typos and other clarity issues in this assignment. How to identify a clarity issue? Take a look at the Piazza questions that are likely to be made, or consider the conversation you have with your classmates and instructors about this assignment.
* Suggest grading criteria. What would make for a strong paper vs a weak paper?
* Tools that can assist in working on this paper.

As usual, pull requests must target the relevant materials. If fixing this document, then submit the change accordingly.



[GitHub dialect]: https://help.github.com/articles/github-flavored-markdown
[Markdown]: http://daringfireball.net/projects/markdown/
[PEPs]: http://www.python.org/dev/peps/
[python-dev]: http://mail.python.org/mailman/listinfo/python-dev
[python-ideas]: http://mail.python.org/mailman/listinfo/python-ideas
[Tiobe top 50]: http://www.tiobe.com/index.php/content/paperinfo/tpci/index.html
