Class {
	#name : #RBSmalllintContext,
	#superclass : #Object,
	#instVars : [
		'class',
		'selector',
		'parseTree',
		'literals',
		'literalSemaphore',
		'literalProcess',
		'selectors',
		'compiledMethod'
	],
	#category : #'Refactoring-Critics'
}

{ #category : #'instance creation' }
RBSmalllintContext class >> newNoCache [
	^self basicNew
]

{ #category : #private }
RBSmalllintContext >> addLiteralsFor: aCompiledMethod [
	2 to: aCompiledMethod numLiterals - 1
		do: [ :index | self checkLiteral: (aCompiledMethod objectAt: index) ]
]

{ #category : #private }
RBSmalllintContext >> buildParseTree [
	| tree |
	tree := self selectedClass parseTreeFor: self selector.
	tree isNil ifTrue: [^RBParser parseMethod: 'method'].
	^tree
]

{ #category : #private }
RBSmalllintContext >> checkLiteral: aLiteral [ 
	(aLiteral isSymbol or: [aLiteral isVariableBinding])
		ifTrue: [literals add: aLiteral]
		ifFalse: 
			[aLiteral class == Array
				ifTrue: [aLiteral do: [:each | self checkLiteral: each]]]
]

{ #category : #accessing }
RBSmalllintContext >> compiledMethod [
	^compiledMethod notNil
		ifTrue: [compiledMethod]
		ifFalse: [compiledMethod := class compiledMethodAt: selector]
]

{ #category : #private }
RBSmalllintContext >> computeLiterals [
	literalSemaphore := Semaphore new.
	literalProcess := [self primitiveComputeLiterals] fork
]

{ #category : #private }
RBSmalllintContext >> computeLiteralsForClass: aClass [ 
	(selectors addAll: aClass selectors) do: 
			[:sel | 
			self computeLiteralsForSelector: sel in: aClass.
			Processor yield]
]

{ #category : #private }
RBSmalllintContext >> computeLiteralsForSelector: aSelector in: aClass [ 
	| method |
	method := aClass compiledMethodAt: aSelector ifAbsent: [nil].
	method isNil ifTrue: [^self].
	self addLiteralsFor: method
]

{ #category : #testing }
RBSmalllintContext >> implements: aSelector [ 
	^self selectors includes: aSelector
]

{ #category : #testing }
RBSmalllintContext >> includesBehaviorNamed: aClassName [
	| current |
	current := self selectedClass.
	[ current isNil ] whileFalse: [
		current name = aClassName
			ifTrue: [ ^ true ].
		current := current superclass ].
	^ false
]

{ #category : #initialization }
RBSmalllintContext >> initialize [
	self computeLiterals
]

{ #category : #accessing }
RBSmalllintContext >> instVarNames [
	^self selectedClass allInstVarNames
]

{ #category : #testing }
RBSmalllintContext >> isAbstract: aClass [ 
	^(aClass isMeta or: 
			[(self literals includes: aClass name)
				or: [self literals includes: (Smalltalk globals associationAt: aClass name)]])
		not
]

{ #category : #accessing }
RBSmalllintContext >> literals [
	literalSemaphore isNil
		ifTrue: 
			[literals isNil ifTrue: 
					[self computeLiterals.
					literalSemaphore wait]]
		ifFalse: [literalSemaphore wait].
	^literals
]

{ #category : #accessing }
RBSmalllintContext >> messages [
    ^(self parseTree sendNodes collect: [ :node | node selector ]) asSet
]

{ #category : #accessing }
RBSmalllintContext >> parseTree [
	^parseTree isNil
		ifTrue: [parseTree := self buildParseTree]
		ifFalse: [parseTree]
]

{ #category : #private }
RBSmalllintContext >> primitiveComputeLiterals [
	| semaphore |
	literals := IdentitySet new: 25000.
	literals addAll: self specialSelectors keys.
	selectors := IdentitySet new.
	RBBrowserEnvironment new
		classesDo: [ :each | self computeLiteralsForClass: each ].
	semaphore := literalSemaphore.
	literalSemaphore := nil.
	self signalProcesses: semaphore.
	^literalProcess := nil
]

{ #category : #printing }
RBSmalllintContext >> printOn: aStream [
	super printOn: aStream.
	self selectedClass isNil ifFalse: [ 
		aStream nextPut: $ ; nextPutAll: self selectedClass name.
		self selector isNil ifFalse: [ 
			aStream nextPutAll: '>>'; print: self selector ] ]
]

{ #category : #accessing }
RBSmalllintContext >> protocol [
	^self selectedClass whichCategoryIncludesSelector: self selector
]

{ #category : #accessing }
RBSmalllintContext >> protocols [
	^Array with: self protocol
]

{ #category : #'initialize-release' }
RBSmalllintContext >> release [
	literalProcess notNil ifTrue: [literalProcess terminate].
	super release
]

{ #category : #accessing }
RBSmalllintContext >> selectedClass [
	^class
]

{ #category : #accessing }
RBSmalllintContext >> selectedClass: anObject [ 
	class := anObject.
	self selector: nil
]

{ #category : #accessing }
RBSmalllintContext >> selector [
	^selector
]

{ #category : #accessing }
RBSmalllintContext >> selector: anObject [ 
	selector := anObject.
	parseTree := compiledMethod := nil
]

{ #category : #accessing }
RBSmalllintContext >> selectors [
	literalSemaphore isNil
		ifTrue: 
			[selectors isNil ifTrue: 
					[self computeLiterals.
					literalSemaphore wait]]
		ifFalse: [literalSemaphore wait].
	^selectors
]

{ #category : #accessing }
RBSmalllintContext >> selfMessages [
    ^self parseTree selfMessages

]

{ #category : #private }
RBSmalllintContext >> signalProcesses: aSemaphore [ 
	aSemaphore isNil ifTrue: [^self].
	[aSemaphore isEmpty] whileFalse: [aSemaphore signal]
]

{ #category : #accessing }
RBSmalllintContext >> sourceCode [
	^self selectedClass sourceCodeAt: self selector ifAbsent: [ '' ].
]

{ #category : #private }
RBSmalllintContext >> specialSelectors [
	| answer |
	answer := IdentityDictionary new.
	(Smalltalk specialSelectors select: [:sel | sel isSymbol]) do:
		[:sel | answer at: sel put: nil.].
	^answer.
]

{ #category : #accessing }
RBSmalllintContext >> superMessages [
    ^self parseTree superMessages

]

{ #category : #testing }
RBSmalllintContext >> uses: anObject [ 
	^self literals includes: anObject
]
