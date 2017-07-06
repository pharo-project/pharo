tests
testQueryingPrimitiveErrorVar
	| tree |
	tree := self parserClass 
				parseMethod: 'clone
	"Answer a shallow copy of the receiver."
	<primitive: 148 error: ec>
	| copy |
	ec == #''insufficient object memory'' ifFalse:
		[|nothing| nothing := nil.
		^ noting].
   copy := nil.
	^ copy'.
	self assert: (tree body whoDefines:'copy') equals: tree body.
	self assert: (tree blockNodes first body whoDefines: 'nothing') equals: tree blockNodes first body.
	self assert: (tree body whoDefines: 'ec') equals: tree.
