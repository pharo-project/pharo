"
SUnit tests for RBReadBeforeWrittenTester
"
Class {
	#name : #RBReadBeforeWrittenTesterTest,
	#superclass : #RBParseTreeTest,
	#category : #'AST-Core-Tests-Visitors'
}

{ #category : #tests }
RBReadBeforeWrittenTesterTest >> testReadBeforeWritten [
	#(#('a ifTrue: [^self]' true) #('self foo. a := b' false) #('condition ifTrue: [a := b] ifFalse: [self foo: a]' true) #('condition ifTrue: [a := b] ifFalse: [self foo]. a isNil' true) #('condition ifTrue: [a := b]. a := c' false) #('[a := b] whileFalse: [a isNil]' false) #('self foo: b' false))
		do: [ :each |
			self assert: ((RBReadBeforeWrittenTester readBeforeWritten: #('a') in: (self parseExpression: each first)) includes: 'a') identicalTo: each last.
			self assert: (RBReadBeforeWrittenTester isVariable: 'a' readBeforeWrittenIn: (self parseExpression: each first)) equals: each last ].
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
			self do: [:each | each & read]')
		do: [ :each |
			| read |
			read := RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: (self parseExpression: each).
			self assert: (read size = 1 and: [ read includes: 'read' ]) ]
]

{ #category : #tests }
RBReadBeforeWrittenTesterTest >> testReadBeforeWritten1 [
	self
		assertEmpty:
			(RBReadBeforeWrittenTester
				variablesReadBeforeWrittenIn:
					(self
						parseMethod:
							'addAll: aCollection
	"Answer aCollection, having added all elements
	 of aCollection to the receiver.

	 Fail if aCollection is not a kind of
	 Collection."

	| newSize elementsSize growSize |
	(newSize := aCollection size * 2) > elements size ifTrue: [self rehash: newSize].
	elementsSize := elements size.
	growSize := elementsSize // 2.
	aCollection do:
			[:newObject |
			| hashIndex element |
			newObject == nil ifFalse:
					[hashIndex := self hashIndexFor: newObject.

					[(element := elements at: hashIndex) == nil
						ifTrue:
							[elements at: hashIndex put: newObject.
							(elementCount := elementCount + 1) > growSize ifTrue:
									[self expand.
									elementsSize := elements size.
									growSize := elementsSize // 2].
							true]
						ifFalse: [element == newObject]]
							whileFalse:
								[(hashIndex := hashIndex + 1) > elementsSize ifTrue: [hashIndex := 1]]]].
	^aCollection'))
]
