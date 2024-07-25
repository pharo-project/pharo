Class {
	#name : 'RBClassDataForRefactoringTest',
	#superclass : 'Object',
	#instVars : [
		'instanceVariable'
	],
	#category : 'Refactoring-DataForTesting-FatClasses',
	#package : 'Refactoring-DataForTesting',
	#tag : 'FatClasses'
}

{ #category : 'lint' }
RBClassDataForRefactoringTest >> asOrderedCollectionNotNeeded [
	self foo addAll: (1 to: 10) asOrderedCollection
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> assignmentInBlock [
	[^self printString] ensure: [self close]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> assignmentInIfTrue [
	| variable |
	self isVariable
		ifTrue: [variable := self]
		ifFalse: [variable := self printString].
	^variable
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> atIfAbsent [
	^ Smalltalk at: #MyTest
		ifAbsent:
			[| collection |
			collection := #().
			Smalltalk at: #MyTest put: collection]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> badMessage [
	self become: String new
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> booleanPrecedence [
	^true & 4 = 45
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> callFoo [
	^self testFoo: 5
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> callMethod [
	^self renameThisMethod: 5
]

{ #category : 'inline' }
RBClassDataForRefactoringTest >> called: anObject on1: aBlock [
	| each |
	each := anObject printString.
	each traceCr.
	aBlock value: each
]

{ #category : 'inline' }
RBClassDataForRefactoringTest >> called: anObject on: aBlock [
	
	anObject printString traceCr; cr.
	aBlock value
]

{ #category : 'inline' }
RBClassDataForRefactoringTest >> caller [
	| anObject |
	anObject := 5.
	self
		called: anObject + 1
		on: [^anObject]
]

{ #category : 'inline' }
RBClassDataForRefactoringTest >> caller1 [
	| anObject |
	anObject := 5.
	self called: anObject + 1
		on1:
			[:each |
			each printString.
			^anObject]
]

{ #category : 'inline' }
RBClassDataForRefactoringTest >> caller2 [
	^(1 to: 10) inject: 1 into: [:sum :each | sum * (self foo: each)]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> collectSelectNotUsed [
	(1 to: 10) select: [:each | each = 4]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> collectionMessagesToExternalObject [
	self someObject collection remove: 10
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> collectionProtocol [
	| newCollection |
	newCollection := OrderedCollection new.
	(1 to: 10) asOrderedCollection do:
			[:each |
			| new |
			new := each * 2.
			newCollection add: new].
	^newCollection
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> consistencyCheck [
	^(1 to: 10) size > 0
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> contains [
	^((1 to: 10) detect: [:each | each > 2] ifNone: [nil]) isNil
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> cruft [
	<haltOrBreakpointForTesting>
	self halt
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> demoExampleCall [
	^ self demoRenameMethod: 1 PermuteArgs: 2
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> demoMethodWithDuplicates [
	| a b result1 result2 answer |
	a := 3.
	b := a + 5.
	result1 := a + b.
	result2 := a + 5.
	answer := result1 + result2 + (a + 5).
	^ answer
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> demoMethodWithDuplicatesInBlocks [

	(1 to: 10) do: [ :i | (i + 1) odd ifTrue: [ (i + 1) doSomething ] ].
	(1 to: 10) do: [ :i |
		(i + 1) even ifTrue: [ (i + 1) doSomethingElse ] ]
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> demoMethodWithDuplicatesInEarlierBlocks [

	(3 to: 10) do: [ :i |
		self someMethod \\ i = 0 ifTrue: [ self performAction: i ] ].
	self someMethod odd ifTrue: [ self performAction ]
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> demoRenameMethod: arg1 PermuteArgs: arg2 [
	self do: arg1.
	self do: arg2.
	^ arg1 > arg2
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> detectContains [
	^(1 to: 10) do: [:each | each > 2 ifTrue: [^each]]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> endTrueFalse [
	self isVariable
		ifTrue:
			[self printString.
			self isVariable printString]
		ifFalse:
			[self printString.
			^4]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> equalNotUsed [
	| string |
	string = '' yourself.
	(1 to: 10) do: [:i | string := i printString].
	^string
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> equalsTrue [
	^true == self
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> exampleCall [
	<sampleInstance>
	^self rename: 1 two: 2
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> extraBlock [
	^[:arg | arg + 43] value: 45
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> fileBlocks [
	| file |
	^
	[file := 'asdf' asFileReference readStream.
	file contents] ensure: [file close]
]

{ #category : 'inline' }
RBClassDataForRefactoringTest >> foo: aValue [
	^(1 to: 10) inject: aValue into: [:sum :each | sum + each]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> fullBlocks [
	^[thisContext]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> guardingClause [
	self isSymbol
		ifFalse:
			[self printString.
			self isSymbol printString]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> ifTrueReturns [
	self isSymbol ifFalse: [^true].
	^false
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> inlineBar: aSymbol [
	^aSymbol isSymbol
]

{ #category : 'inline' }
RBClassDataForRefactoringTest >> inlineComponent [
	| a |
	a := 5.
	^a class
		superclass;
		hasImmediateInstances;
		yourself
]

{ #category : 'inline' }
RBClassDataForRefactoringTest >> inlineFailed [
	| x y q |
	x := 5.
	y := 10.
	q := x + 1 fooMax: y.
	^q
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> inlineFoo: aBlock [
	| bar baz asdf |
	bar := aBlock value: self.
	baz := bar + bar.
	asdf := baz + bar.
	^asdf
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> inlineJunk [
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

{ #category : 'inline' }
RBClassDataForRefactoringTest >> inlineLast [
	5 = 3 ifTrue: [^self caller] ifFalse: [^self caller2]
]

{ #category : 'inline' }
RBClassDataForRefactoringTest >> inlineMax [
	| x y q |
	x := 5.
	y := 10.
	q := x + 1 max: y.
	^q
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> inlineMethod [
	| temp |
	temp := self
				foo;
				inlineMethod;
				bar.
	^temp
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> inlineParameterMethod: aSymbol [
	^aSymbol isSymbol
]

{ #category : 'inline' }
RBClassDataForRefactoringTest >> inlineTemporary [
	| temp |
	self isNil ifTrue: [temp := 4].
	^temp
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> justSendsSuper [
	super justSendsSuper
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> literalArrayCharacters [
	^#($a $b $c) includes: $a
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> literalArrayWithTrueFalseOrNil [
	| a b c |
	a := #(true false nil).
	b := #(#true #false #nil).
	c := {true. false. nil}.
	^{a. b. c}
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> literalArrayWithTrueFalseOrNil2 [
	| b c |
	b := #(#true #false #nil).
	c := #(#true (#true #false #nil) #false #nil).
^b
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> longMethods [
	self printString.
	self printString.
	self isVariable ifTrue: [self printString].
	self isVariable ifTrue: [self printString].
	self isVariable ifTrue: [self printString].
	self isVariable ifTrue: [self printString]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> minMax [
	"Bug in 3<5 ifTrue: [3]  ifFalse: [5]"

^3<5 ifTrue: [3]  ifFalse: [5]
"	| var |
	var := 4.
	var < 5 ifTrue: [var := 5].
	^var"
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> missingYourself [
	^(OrderedCollection new)
		add: 1;
		add: 2;
		removeFirst
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> modifiesCollection [
	| collection |
	collection := (1 to: 10) asOrderedCollection.
	collection do: [:each | each > 5 ifTrue: [collection remove: each]].
	^collection
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> moveDefinition [
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

{ #category : 'inline' }
RBClassDataForRefactoringTest >> multipleCalls [
	self caller2.
	self caller2
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> noMoveDefinition [
	| temp |
	^(self collect:
			[:each |
			temp := each printString.
			temp , temp])
		select: [:each | temp := each size + temp]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> precedence [
	^self isArray ifFalse: [self block + 5 * 34] ifTrue: [self printString = 10]
]

{ #category : 'accessing' }
RBClassDataForRefactoringTest >> referencesConditionFor: aClass [
	| environment association |
	^(RBCondition withBlock:
			[association := Smalltalk globals associationAt: aClass name
						ifAbsent: [self refactoringError: 'Could not find class'].
			environment := (self environment referencesTo: association)
						| (self environment referencesTo: aClass name).
			environment isEmpty])
		errorMacro: aClass , ' is referenced.<n>Browse references?';
		errorBlock: [environment openEditor];
		yourself
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> refersToClass [
	^ RBClassDataForRefactoringTest
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> release [
	self printString
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> rename: this two: argumentMethod [
	^self printString , this , argumentMethod
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> renameThisMethod: anArg [
	^self
]

{ #category : 'rendering' }
RBClassDataForRefactoringTest >> renderContentOn: html [
	html
		form: [ html text: 'Name:'.
			html textInput on: #name of: self contact.


			html break.
			html text: 'Email address:'.
			html textInput on: #emailAddress of: self contact.

			html break.
			html text: 'Example:'.

			html textInput on: #example of: self contact.
			html break.].
	html text: 'Last:'.
			html textInput on: #last of: self contact.
					html break
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> returnInEnsure [
	[self error: 'asdf'] ensure: [^4]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> returnsBooleanAndOther [
	self isVariable ifTrue: [^false].
	self printString
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> returnsIfTrue [
	^self isNil ifTrue: [4]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> searchingLiteral [
	^self printString = #a or: [#() = self printString | ( #() == self printString)]
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> selectorNotReferenced [
	^self selectorNotReferenced + 4
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> sendInlineBar [
	^ self inlineBar: #example1
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> sendInlineBar2 [
	^ self inlineBar: 'example2'
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> sendInlineParameterMethod [
	^self inlineParameterMethod: #(#asdf)
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> sendsDifferentSuper [
	super printString
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> sizeCheck [

	self isEmpty
		ifFalse: [ self do: [ :each | each traceCr ] ]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> stringConcatenation [
	| string |
	string := '' yourself.
	(1 to: 10) do: [:i | string := string , i printString].
	^string
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> symbolReference [
	^#(#renameThisMethod: #(4 #renameThisMethod:))
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> tempVarOverridesInstVar [
	| temporaryVariable |
	temporaryVariable := 4.
	^temporaryVariable
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> tempsReadBeforeWritten [
	| temp |
	self isVariable ifTrue: [temp := 4].
	^temp
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> testFoo: anObject [
	^self class + anObject
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> testMethod [
	^self class
]

{ #category : 'tests' }
RBClassDataForRefactoringTest >> testMethod1 [
	^self testMethod1
		, ([:each | each testMethod1] value: #(#(#testMethod1) 2 #testMethod1))
]

{ #category : 'rendering' }
RBClassDataForRefactoringTest >> textInput: html name: aString symbol: aSymbol [
	html text: aString.
	html textInput on: aSymbol of: self contact.
	html break
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> threeElementPoint [
	^5 @ 5 + 6 @ 6
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> toDo [
	1 to: self size do: [:i | (self at: i) printString]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> toDoCollect [
	| array |
	array := Array new: 10.
	1 to: 10 do: [:i | array at: i put: i * i].
	^array
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> toDoWithIncrement [
	| counter |
	counter := 0.
	1 to: 10
		by: 2
		do: [:i | counter := counter + 2].
	^counter
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> usesAdd [
	^(1 to: 10) asOrderedCollection addAll: (11 to: 20)
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> variableAssignedLiteral [
	instanceVariable := #()
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> whileTrue [
	| i |
	i := 1.
	[i < self size] whileTrue:
			[(self at: i) printString.
			i := i + 1]
]

{ #category : 'lint' }
RBClassDataForRefactoringTest >> yourselfNotUsed [
	self
		printString;
		printString;
		yourself
]
