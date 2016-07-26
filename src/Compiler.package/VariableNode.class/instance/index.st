testing
index
	"This code attempts to reconstruct the index from its encoding in code."
	code < 0 ifTrue:[^nil].
	code > 256 ifTrue:
		[[index = (code \\ 256)] assert.
		^code \\ 256].
	code >= (CodeBases at: self type) ifTrue:
		[[index = (code - (CodeBases at: self type))] assert.
		^code - (CodeBases at: self type)].
	[index = (code - self type)] assert.
	^code - self type