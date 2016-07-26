replacing
replaceMethodSource: aNode 
	"We are being replaced with aNode -- if possible try to perform an in place edit of the source."

	| method |
	method := self methodNode.
	method notNil ifTrue: [method map: self to: aNode].
	aNode parent: self parent.
	[self replaceSourceWith: aNode] on: Error
		do: 
			[:ex | 
			self clearReplacements.
			ex return]