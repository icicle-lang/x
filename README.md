<div align="center">

# X
### A collection of extra combinator libraries.

[![Build Status](https://api.travis-ci.com/icicle-lang/x.svg?branch=master)](https://travis-ci.com/icicle-lang/x)

</div>

Convention for module names is `X.<Module.You.Are.Shadowing>`.

Not that this is possible, but say lens is missing a combinator, you may want
to add `x-lens` which would have a module `X.Control.Lens`.

It probably makes sense for these things to eventually fold back into
upstream libraries. However, this makes a good playground for adding
things a bit quicker, a spot for when upstream doesn't play nice, or
when it introduces a dependency that the original library might not
have/need/want.
