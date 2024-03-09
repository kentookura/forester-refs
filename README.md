# forester-refs

Exploring some of the ideas presented in [A Core Calculus for
Documents](https://arxiv.org/abs/2310.04368) with forester.

The reference implementation by C & K assumes we are working on an article in
which we refer to various sections contained in the same article/file. We
implement an analogous `addrs_at_depth` function which verifies that the
transclusions are valid. I would like to carry out this computation by
traversing the document in question once to scan for transclusions, and then
only using the import graph. However, it seems that the list returned by
`Gph.pred` is not in the order in which the transclusions appear in the tree.
Thus a correct implementation needs to scan the body of each transcluded tree. 
