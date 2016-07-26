tests
testReadBeforeWritten
	#(('a ifTrue: [^self]' true ) ('self foo. a := b' false ) ('condition ifTrue: [a := b] ifFalse: [self foo: a]' true ) ('condition ifTrue: [a := b] ifFalse: [self foo]. a isNil' true ) ('condition ifTrue: [a := b]. a := c' false ) ('[a := b] whileFalse: [a isNil]' false ) ('self foo: b' false ) ) do: 
		[:each | 
		self assert: ((RBReadBeforeWrittenTester readBeforeWritten: #('a' ) in: (RBParser parseExpression: each first))
				includes: 'a')
				== each last.
		self assert: (RBReadBeforeWrittenTester isVariable: 'a' readBeforeWrittenIn: (RBParser parseExpression: each first))
				= each last].
	#('| temp read written written1 |
			read ifTrue: [^self].
			written1 := self foo ifFalse: [written := true] ifTrue: [written := false].
			[temp := true] whileTrue: [temp notNil & written].
			^temp' '| read written |
			self foo ifTrue: [written := true] ifFalse: [written := false].
			self foo ifTrue: [read := true].
			^read' '| read written |
			self foo do: [:i | i].
			[| i | i := 1. i == 1] whileFalse: [read notNil]' '| written |
			[written := 2] whileFalse.
			self do: [:each | | read | each & read]' '| read |
			self do: [:each | read := each].
			self do: [:each | each & read]' ) do: 
		[:each | 
		| read | 
		read := RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: (RBParser parseExpression: each).
		self assert: (read size = 1 and: [read includes: 'read'])]