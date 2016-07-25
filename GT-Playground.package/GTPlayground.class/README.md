Playground is the correspondent of a classic Smalltalk Workspace. The problem with the name Workspace is that it implies that work should be carried out in this space, while this is not a best practice. Playground describes better the intention of providing a place in which we can quickly play with some code.

The model behind a Playground instance is a PlayPage object.

!!Use cases
The Playground can be used in two ways

# As a place to construct and try out code snippets.
# as an entry point into an inspection process.

In both cases, the ability of diving into objects to the right is an important feature.

!!Running
[ [ [ 
	self open.
 ] ] ]