reflectivity
emitPrepareLinkAfter: aNode
	"We need to emit the first part of the block for the ensure: wrap here"
	| copied |
	copied := #().
	aNode hasMetalinkAfter ifFalse: [^self].
	self emitPreamble: aNode.
	aNode allAfterAreWeak ifTrue: [ ^self ].
	aNode isMethod ifTrue: [ copied := aNode argumentNames, aNode tempNames ].
	methodBuilder
		pushClosureCopyCopiedValues: copied
		args: #()
		jumpTo: #block