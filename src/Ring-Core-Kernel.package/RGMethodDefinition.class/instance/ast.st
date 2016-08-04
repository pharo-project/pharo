accessing
ast
	self isActive
		ifTrue: [ ^ self compiledMethod ast ].
	^  self sourceCode ifNotNil: [ ^ self astFromSource ]