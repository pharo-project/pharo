Class {
	#name : 'CompiledMethodEvaluationTest',
	#superclass : 'TestCase',
	#instVars : [
		'someClass'
	],
	#category : 'Kernel-Extended-Tests-Methods',
	#package : 'Kernel-Extended-Tests',
	#tag : 'Methods'
}

{ #category : 'tests - performing' }
CompiledMethodEvaluationTest >> a1: a1 a2: a2 a3: a3 a4: a4 a5: a5 a6: a6 a7: a7 a8: a8 a9: a9 a10: a10 a11: a11 a12: a12 a13: a13 a14: a14 a15: a15 [
	"I'm a method with the maximum size of arguments that can be executed via normal send but crash on perform :)"

	^ a1 + a2 - a2
]

{ #category : 'running' }
CompiledMethodEvaluationTest >> setUp [
	super setUp.
	someClass := SomeClassForCompiledMethodTests
]

{ #category : 'accessing' }
CompiledMethodEvaluationTest >> someClass [

	^ someClass 
]

{ #category : 'running' }
CompiledMethodEvaluationTest >> tearDown [ 
	someClass compile: 'returnTrue
	^true'.
	super tearDown
]

{ #category : 'tests - instance variable' }
CompiledMethodEvaluationTest >> testAccessesField [
	| method |
	method := self someClass compiledMethodAt: #readX.
	self assert: (method accessesField: 4).

	method := self someClass compiledMethodAt: #readXandY.
	self assert: (method accessesField: 5).


	"read is not write"
	method := self someClass compiledMethodAt: #writeX.
	self assert: (method accessesField: 4).

	method := self someClass compiledMethodAt: #writeXandY.
	self assert: (method accessesField: 4).

	method := self someClass compiledMethodAt: #writeXandY.
	self assert: (method accessesField: 5)
]

{ #category : 'tests - testing' }
CompiledMethodEvaluationTest >> testIsAbstract [

	self assert: (self someClass >> #abstractMethod) isAbstract.
	self deny: (self someClass >> #nonAbstractMethod) isAbstract.
	self deny: (self someClass >> #shouldNotImplementMethod) isAbstract
]

{ #category : 'tests - testing' }
CompiledMethodEvaluationTest >> testIsQuick [

	| method |
	method := self someClass compiledMethodAt: #returnTrue.
	self assert: method isQuick.

	method := self someClass compiledMethodAt: #returnPlusOne:.
	self deny: method isQuick
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeAllInOne [
	| method |
	method := self someClass >> #locComplex.
	self assert: method linesOfCode equals: 14
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeEmptyLineInTheEnd [
	| method |
	method := self someClass >> #locEmptyLineInTheEnd.
	self assert: method linesOfCode equals: 4
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeEmptyLineInTheMiddle [
	| method |
	method := self someClass >> #locEmptyLineInTheMiddle.
	self assert: method linesOfCode equals: 4
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeEmptyLineWithTabInTheEnd [
	| method |
	method := self someClass >> #locEmptyLineWithTabInTheEnd.
	self assert: method linesOfCode equals: 4
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeEmptyLineWithTabInTheMiddle [
	| method |
	method := self someClass >> #locEmptyLineWithTabInTheMiddle.
	self assert: method linesOfCode equals: 4
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeEmptyMethod [
	| method |
	method := self someClass >> #locEmptyMethod.
	self assert: method linesOfCode equals: 1
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeEmptyMethodWithNewline [
	| method |
	method := self someClass >> #locEmptyMethodWithNewline.
	self assert: method linesOfCode equals: 1
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeEmptyMethodWithNewlineAndTab [
	| method |
	method := self someClass >> #locEmptyMethodWithNewlineAndTab.
	self assert: method linesOfCode equals: 1
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeMultilineComment [
	| method |
	method := self someClass >> #locMultilineComment.
	self assert: method linesOfCode equals: 4
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeMultilineCommentWithoutWhitespace [
	| method |
	method := self someClass >> #locMultilineCommentWithoutWhitespace.
	self assert: method linesOfCode equals: 6
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeMultilineMethodComment [
	| method |
	method := self someClass >> #locMultilineMethodComment.
	self assert: method linesOfCode equals: 4
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeMultilineMethodCommentWithoutWhitespace [
	| method |
	method := self someClass >> #locMultilineMethodCommentWithoutWhitespace.
	self assert: method linesOfCode equals: 3
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeSimpleCase [
	| method |
	method := self someClass >> #locSimple.
	self assert: method linesOfCode equals: 6
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeSingleLineComment [
	| method |
	method := self someClass >> #locSingleLineComment.
	self assert: method linesOfCode equals: 3
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeSingleLineCommentWithoutWhitespace [
	| method |
	method := self someClass >> #locSingleLineCommentWithoutWhitespace.
	self assert: method linesOfCode equals: 3
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeSingleLineMethodComment [
	| method |
	method := self someClass >> #locSingleLineMethodComment .
	self assert: method linesOfCode equals: 3
]

{ #category : 'tests - linesofcode' }
CompiledMethodEvaluationTest >> testLinesOfCodeSingleLineMethodCommentWithoutWhitespace [
	| method |
	method := self someClass >> #locSingleLineMethodCommentWithoutWhitespace.
	self assert: method linesOfCode equals: 3
]

{ #category : 'tests - performing' }
CompiledMethodEvaluationTest >> testPerformCanExecutelongMethodWithTemps [
	"the perform: primitive reuses the context of the method calling it. The primitive adds performed selector arguments to the context variables list. So this means that you can execute some methods but not performed them if the calling methods defined too many temps "

	| temp1 temp2 temp3 |
	temp1 := 33.
	temp2 := 666.
	temp3 := 42.
	self assert: (self perform: #a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15: withArguments: #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) equals: 1.
	self assert: (self class>>#testPerformCanExecutelongMethodWithTemps) frameSize equals: CompiledMethod smallFrameSize.
	self assert: (self class>>#a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15:) frameSize equals: CompiledMethod fullFrameSize
]

{ #category : 'tests - performing' }
CompiledMethodEvaluationTest >> testPerformInSuperclassCanExecutelongMethodWithTemps [
	"the perform: primitive reuses the context of the method calling it. The primitive adds performed selector arguments to the context variables list. So this means that you can execute some methods but not performed them if the calling methods defined too many temps "

	| temp1 temp2 temp3 |
	temp1 := 33.
	temp2 := 666.
	temp3 := 42.
	self assert: (self perform: #a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15: withArguments: #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) inSuperclass: self class) equals: 1
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethodEvaluationTest >> testPragmaAt [

	| method |
	method := self someClass >> #methodWithPragma.
	self assert: (method pragmaAt: #bebou) selector equals: #bebou.
	self assert: (method pragmaAt: #hello) equals: nil.
	method propertyAt: #hello put: true.
	"Pragmas are not Properties!"
	self assert: (method pragmaAt: #hello) equals: nil
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethodEvaluationTest >> testProperties [

	| method tmp |
	method := self someClass >> #returnTrue.
	
	"No property. Unlike classic collections, nil is returned on absence"
	self deny: method hasProperties.
	self deny: (method hasProperty: #doesNotExist).
	self assert: (method propertyAt: #doesNotExist) equals: nil.
	self assert: (method propertyAt: #doesNotExist ifAbsent: [ #tag ]) equals: #tag.
	self assert: (method removeProperty: #doesNotExist) equals: nil.
	self assert: (method removeProperty: #doesNotExist ifAbsent: [ #tag ]) equals: #tag.
	
	"Add a property"
	self assert: (method propertyAt: #doesNotExist put: #yesItDoes) equals: #yesItDoes.
	self assert: method hasProperties.
	self assert: (method hasProperty: #doesNotExist).
	self assert: (method propertyAt: #doesNotExist) equals: #yesItDoes.
	self assert: (method propertyAt: #doesNotExist ifAbsent: [ #tag ]) equals: #yesItDoes.

	"Remove it"
	self assert: (method removeProperty: #doesNotExist) equals: #yesItDoes.
	self deny: method hasProperties.
	self assert: (method removeProperty: #doesNotExist) equals: nil.

	"Add it back"
	self assert: (method propertyAt: #doesNotExist ifAbsentPut: [ #yesItDoesAgain ]) equals: #yesItDoesAgain.
	self assert: method hasProperties.
	self assert: (method propertyAt: #doesNotExist) equals: #yesItDoesAgain.

	"Add a second one"
	self assert: (method propertyAt: #doesExist put: #yesItDoes) equals: #yesItDoes.
	
	tmp := OrderedCollection new.
	method propertyKeysAndValuesDo: [ :key :value | tmp add: { key . value } ].
	self assertCollection: tmp hasSameElements: #( #( #doesNotExist #yesItDoesAgain ) #(#doesExist #yesItDoes) ).
	
	"Remove both"
	method removeProperty: #doesNotExist.
	self assert: method hasProperties.
	self assert: (method propertyAt: #doesNotExist) equals: nil.
	self assert: (method propertyAt: #doesExist) equals: #yesItDoes.
	method removeProperty: #doesExist.
	self deny: method hasProperties
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethodEvaluationTest >> testPropertyAtIfPresentDoNotClashWithPragmas [
	"This is a regression test because #propertyAt:ifPresent: was returning the pragmas of the same name."

	| method |
	method := self someClass >> #methodWithPragma.

	[
	method propertyAt: #bebou ifPresent: [ :value | self fail: 'There should be no property on this method.' ].

	method propertyAt: #bebou put: true.

	self assert: (method propertyAt: #bebou ifPresent: [ :value | value ]) ] ensure: [ method removeProperty: #bebou ]
]

{ #category : 'tests - instance variable' }
CompiledMethodEvaluationTest >> testReadsField [
	| method |
	method := self someClass compiledMethodAt: #readX.
	self assert: (method readsField: 4).

	method := self someClass compiledMethodAt: #readXandY.
	self assert: (method readsField: 5).


	"read is not write"
	method := self someClass compiledMethodAt: #writeX.
	self deny: (method readsField: 4).

	method := self someClass compiledMethodAt: #writeXandY.
	self deny: (method readsField: 4).

	method := self someClass compiledMethodAt: #writeXandY.
	self deny: (method readsField: 5)
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethodEvaluationTest >> testRemovePropertyIfAbsentRemovesAdditionalMethodStateIfEmpty [

	| method |
	
	method := self someClass >> #returnTrue.
	method propertyAt: #aTestProperty put: 42.
	
	self assert: (method hasProperty: #aTestProperty).
	self assert: method penultimateLiteral isMethodProperties.
	self assert: method penultimateLiteral size equals: 1.
	
	method removeProperty: #aTestProperty ifAbsent: [ self fail ].
	
	self deny: (method hasProperty: #aTestProperty).
	self deny: method penultimateLiteral isMethodProperties.

]

{ #category : 'accessing - pragmas & properties' }
CompiledMethodEvaluationTest >> testRemovePropertyRemovesAdditionalMethodStateIfEmpty [

	| method |
	
	method := self someClass >> #returnTrue.
	method propertyAt: #aTestProperty put: 42.
	
	self assert: (method hasProperty: #aTestProperty).
	self assert: method penultimateLiteral isMethodProperties.
	self assert: method penultimateLiteral size equals: 1.
	
	method removeProperty: #aTestProperty.
	
	self deny: (method hasProperty: #aTestProperty).
	self deny: method penultimateLiteral isMethodProperties.

]

{ #category : 'tests - testing' }
CompiledMethodEvaluationTest >> testUsesUndeclareds [
	| method |
	method := self someClass compiler source: 'x ^x'; compileOrFail.
	self deny: method usesUndeclareds.
	method := self someClass compiler permitFaulty: true; source: 'z ^z'; compileOrFail.
	self assert: method usesUndeclareds.
	method := self someClass compiler permitFaulty: true; source: 'msg self in: [ :anObject | var1 := 1 ]'; compileOrFail.
	self assert: method usesUndeclareds
]

{ #category : 'tests - evaluating' }
CompiledMethodEvaluationTest >> testValueWithReceiver [

	| method value |

	method := self someClass compiledMethodAt: #returnTrue.

	value := method valueWithReceiver: nil .
	self assert: value equals: true.
]

{ #category : 'tests - evaluating' }
CompiledMethodEvaluationTest >> testValueWithReceiverArguments [

	| method value |

	method := self someClass compiledMethodAt: #returnTrue.

	value := method valueWithReceiver: nil.
	self assert: value equals: true.

	method := self someClass compiledMethodAt: #returnPlusOne:.
	value := method valueWithReceiver: nil arguments: #(1).
	self assert: value equals: 2
]

{ #category : 'tests - instance variable' }
CompiledMethodEvaluationTest >> testWritesField [
	| method |
	method := self someClass compiledMethodAt: #writeX.
	self assert: (method writesField: 4).

	method := self someClass compiledMethodAt: #writeXandY.
	self assert: (method writesField: 4).

	method := self someClass compiledMethodAt: #writeXandY.
	self assert: (method writesField: 5).

	"write is not read"

	method := self someClass compiledMethodAt: #readX.
	self deny: (method writesField: 4).

	method := self someClass compiledMethodAt: #readXandY.
	self deny: (method writesField: 4).

	method := self someClass compiledMethodAt: #readXandY.
	self deny: (method writesField: 5)
]

{ #category : 'tests - testing' }
CompiledMethodEvaluationTest >> testWritesUndeclared [
	| method |
	
	"x is an ivar"
	method := self someClass compiler source: 'x ^ x := 0'; compileOrFail.
	self deny: method usesUndeclareds.
	self assert: (self someClass new executeMethod: method) equals: 0.

	"undeclaredxyz is not declared"
	method := self someClass compiler permitFaulty: true; source: 'z ^ undeclaredxyz := 1'; compileOrFail.
	self assert: method usesUndeclareds.
	self should: [ self someClass new executeMethod: method ] raise: UndeclaredVariableWrite.
	self
		assert:
			([ self someClass new executeMethod: method ]
				on: UndeclaredVariableWrite
				do: [:e | e resume])
		equals: 1.

	"check same behavior in blocks"
	method := self someClass compiler permitFaulty: true; source: 'msg ^ self in: [ :anObject | undeclaredxyz := 2 ]'; compileOrFail.
	self assert: method usesUndeclareds.
	self should: [ self someClass new executeMethod: method ] raise: UndeclaredVariableWrite.
	self
		assert:
			([ self someClass new executeMethod: method ]
				on: UndeclaredVariableWrite
				do: [:e | e resume])
		equals: 2
]
