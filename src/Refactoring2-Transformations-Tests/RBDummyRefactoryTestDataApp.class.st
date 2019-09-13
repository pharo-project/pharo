Class {
	#name : #RBDummyRefactoryTestDataApp,
	#superclass : #Object,
	#instVars : [
		'temporaryVariable'
	],
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #lint }
RBDummyRefactoryTestDataApp >> asOrderedCollectionNotNeeded [
	self foo addAll: (1 to: 10) asOrderedCollection
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> assignmentInBlock [
	[^self printString] ensure: [self close]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> assignmentInIfTrue [
	| variable |
	self isVariable 
		ifTrue: [variable := self]
		ifFalse: [variable := self printString].
	^variable
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> atIfAbsent [
	^ Smalltalk at: #MyTest
		ifAbsent: 
			[| collection |
			collection := #().
			Smalltalk at: #MyTest put: collection]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> badMessage [
	self become: String new
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> booleanPrecedence [
	^true & 4 = 45
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> callFoo [
	^self testFoo: 5
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> callMethod [
	^self renameThisMethod: 5
]

{ #category : #inline }
RBDummyRefactoryTestDataApp >> called: anObject on1: aBlock [ 
	| each |
	each := anObject printString.
	self traceCr: each.
	aBlock value: each
]

{ #category : #inline }
RBDummyRefactoryTestDataApp >> called: anObject on: aBlock [ 
	self traceCr: anObject printString.
	aBlock value
]

{ #category : #inline }
RBDummyRefactoryTestDataApp >> caller [
	| anObject |
	anObject := 5.
	self 
		called: anObject + 1
		on: [^anObject]
]

{ #category : #inline }
RBDummyRefactoryTestDataApp >> caller1 [
	| anObject |
	anObject := 5.
	self called: anObject + 1
		on1: 
			[:each | 
			each printString.
			^anObject]
]

{ #category : #inline }
RBDummyRefactoryTestDataApp >> caller2 [
	^(1 to: 10) inject: 1 into: [:sum :each | sum * (self foo: each)]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> collectSelectNotUsed [
	(1 to: 10) select: [:each | each = 4]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> collectionMessagesToExternalObject [
	self someObject collection remove: 10
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> collectionProtocol [
	| newCollection |
	newCollection := OrderedCollection new.
	(1 to: 10) asOrderedCollection do: 
			[:each | 
			| new |
			new := each * 2.
			newCollection add: new].
	^newCollection
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> consistencyCheck [
	^(1 to: 10) size > 0
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> contains [
	^((1 to: 10) detect: [:each | each > 2] ifNone: [nil]) isNil
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> cruft [
	<haltOrBreakpointForTesting>
	self halt
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> demoExampleCall [
	^ self demoRenameMethod: 1 PermuteArgs: 2
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> demoMethodWithDuplicates [
	| a b result1 result2 answer |
	a := 3.
	b := a + 5.
	result1 := a + b.
	result2 := a + 5.
	answer := result1 + result2 + (a + 5).
	^ answer
]

{ #category : #'as yet unclassified' }
RBDummyRefactoryTestDataApp >> demoRenameMethod: arg1 PermuteArgs: arg2 [
	self do: arg1. 
	self do: arg2.
	^ arg1 > arg2
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> detectContains [
	^(1 to: 10) do: [:each | each > 2 ifTrue: [^each]]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> endTrueFalse [
	self isVariable 
		ifTrue: 
			[self printString.
			self isVariable printString]
		ifFalse: 
			[self printString.
			^4]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> equalNotUsed [
	| string |
	string = '' yourself.
	(1 to: 10) do: [:i | string := i printString].
	^string
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> equalsTrue [
	^true == self
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> exampleCall [
	^self rename: 1 two: 2
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> extraBlock [
	^[:arg | arg + 43] value: 45
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> fileBlocks [
	| file |
	^
	[file := 'asdf' asFileReference readStream.
	file contents] ensure: [file close]
]

{ #category : #inline }
RBDummyRefactoryTestDataApp >> foo: aValue [
	^(1 to: 10) inject: aValue into: [:sum :each | sum + each]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> fullBlocks [
	^[thisContext]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> guardingClause [
	self isSymbol 
		ifFalse: 
			[self printString.
			self isSymbol printString]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> ifTrueReturns [
	self isSymbol ifFalse: [^true].
	^false
]

{ #category : #inline }
RBDummyRefactoryTestDataApp >> inlineComponent [
	| a |
	a := 5.
	^a class
		superclass;
		hasImmediateInstances;
		yourself
]

{ #category : #inline }
RBDummyRefactoryTestDataApp >> inlineFailed [
	| x y q |
	x := 5.
	y := 10.
	q := x + 1 fooMax: y.
	^q
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> inlineFoo: aBlock [ 
	| bar baz asdf |
	bar := aBlock value: self.
	baz := bar + bar.
	asdf := baz + bar.
	^asdf
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> inlineJunk [
	| asdf |
	asdf := self inlineFoo: 
					[:each | 
					| temp |
					temp := each.
					temp , temp].
	^asdf foo: 
			[:bar | 
			| baz |
			baz := bar.
			baz * baz]
]

{ #category : #inline }
RBDummyRefactoryTestDataApp >> inlineLast [
	5 = 3 ifTrue: [^self caller] ifFalse: [^self caller2]
]

{ #category : #inline }
RBDummyRefactoryTestDataApp >> inlineMax [
	| x y q |
	x := 5.
	y := 10.
	q := x + 1 max: y.
	^q
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> inlineMethod [
	| temp |
	temp := self
				foo;
				inlineMethod;
				bar.
	^temp
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> inlineParameterMethod: aSymbol [
	^aSymbol isSymbol
]

{ #category : #inline }
RBDummyRefactoryTestDataApp >> inlineTemporary [
	| temp |
	self isNil ifTrue: [temp := 4].
	^temp
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> justSendsSuper [
	super justSendsSuper
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> literalArrayCharacters [
	^#($a $b $c) includes: $a
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> literalArrayWithTrueFalseOrNil [
	| a b c |
	a := #(true false nil).
	b := #(#true #false #nil).
	c := {true. false. nil}.
	^{a. b. c}
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> literalArrayWithTrueFalseOrNil2 [
	| b c |
	b := #(#true #false #nil).
	c := #(#true (#true #false #nil) #false #nil).
^b
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> longMethods [
	self printString.
	self printString.
	self isVariable ifTrue: [self printString].
	self isVariable ifTrue: [self printString].
	self isVariable ifTrue: [self printString].
	self isVariable ifTrue: [self printString]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> minMax [
	"Bug in 3<5 ifTrue: [3]  ifFalse: [5]"

^3<5 ifTrue: [3]  ifFalse: [5]
"	| var |
	var := 4.
	var < 5 ifTrue: [var := 5].
	^var"
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> missingYourself [
	^(OrderedCollection new)
		add: 1;
		add: 2;
		removeFirst
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> modifiesCollection [
	| collection |
	collection := (1 to: 10) asOrderedCollection.
	collection do: [:each | each > 5 ifTrue: [collection remove: each]].
	^collection
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> moveDefinition [
	| temp |
	^(self collect: 
			[:each | 
			temp := each printString.
			temp , temp]) 
		select: 
			[:each | 
			temp := each size.
			temp odd]
]

{ #category : #inline }
RBDummyRefactoryTestDataApp >> multipleCalls [
	self caller2.
	self caller2
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> noMoveDefinition [
	| temp |
	^(self collect: 
			[:each | 
			temp := each printString.
			temp , temp]) 
		select: [:each | temp := each size + temp]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> precedence [
	^self isArray ifFalse: [self block + 5 * 34] ifTrue: [self printString = 10]
]

{ #category : #accessing }
RBDummyRefactoryTestDataApp >> referencesConditionFor: aClass [ 
	| environment association |
	^(RBCondition withBlock: 
			[association := Smalltalk associationAt: aClass name
						ifAbsent: [self refactoringError: 'Could not find class'].
			environment := (self environment referencesTo: association) 
						| (self environment referencesTo: aClass name).
			environment isEmpty])
		errorMacro: aClass , ' is referenced.<n>Browse references?';
		errorBlock: [environment openEditor];
		yourself
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> refersToClass [
	^ RBRefactoryTestDataApp
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> release [
	self printString
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> rename: this two: argumentMethod [
	^self printString , this , argumentMethod
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> renameThisMethod: anArg [
	^self
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> returnInEnsure [
	[self error: 'asdf'] ensure: [^4]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> returnsBooleanAndOther [
	self isVariable ifTrue: [^false].
	self printString
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> returnsIfTrue [
	^self isNil ifTrue: [4]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> searchingLiteral [
	^self printString = #a or: [#() = self printString | ( #() == self printString)]
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> selectorNotReferenced [
	^self selectorNotReferenced + 4
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> sendInlineParameterMethod [
	^self inlineParameterMethod: #(#asdf)
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> sendsDifferentSuper [
	super printString
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> sizeCheck [
	self isEmpty
		ifFalse: [self do: 
					[:each | 
					self traceCr: each]]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> stringConcatenation [
	| string |
	string := '' yourself.
	(1 to: 10) do: [:i | string := string , i printString].
	^string
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> symbolReference [
	^#(#renameThisMethod: #(4 #renameThisMethod:))
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> tempVarOverridesInstVar [
	| temporaryVariable |
	temporaryVariable := 4.
	^temporaryVariable
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> tempsReadBeforeWritten [
	| temp |
	self isVariable ifTrue: [temp := 4].
	^temp
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> testFoo: anObject [
	^self class + anObject
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> testMethod [
	^self class
]

{ #category : #test }
RBDummyRefactoryTestDataApp >> testMethod1 [
	^self testMethod1 
		, ([:each | each testMethod1] value: #(#(#testMethod1) 2 #testMethod1))
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> threeElementPoint [
	^5 @ 5 + 6 @ 6
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> toDo [
	1 to: self size do: [:i | (self at: i) printString]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> toDoCollect [
	| array |
	array := Array new: 10.
	1 to: 10 do: [:i | array at: i put: i * i].
	^array
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> toDoWithIncrement [
	| counter |
	counter := 0.
	1 to: 10
		by: 2
		do: [:i | counter := counter + 2].
	^counter
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> usesAdd [
	^(1 to: 10) asOrderedCollection addAll: (11 to: 20)
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> variableAssignedLiteral [
	temporaryVariable := #() 
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> whileTrue [
	| i |
	i := 1.
	[i < self size] whileTrue: 
			[(self at: i) printString.
			i := i + 1]
]

{ #category : #lint }
RBDummyRefactoryTestDataApp >> yourselfNotUsed [
	self
		printString;
		printString;
		yourself
]
