Class {
	#name : #RBFindAndReplaceRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'method',
		'selector',
		'replacesAllHierarchy',
		'matchNodes'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'as yet unclassified' }
RBFindAndReplaceRefactoring class >> find: aMethod of: aClass inAllHierarchy: aBoolean [
	^ self new 
		find: aMethod 
		of: aClass
		inAllHierarchy: aBoolean;
		yourself
]

{ #category : #'as yet unclassified' }
RBFindAndReplaceRefactoring class >> model: aModel find: aMethod of: aClass inAllHierarchy: aBoolean [
	^ (self new)
		model: aModel; 
		find: aMethod 
		of: aClass
		inAllHierarchy: aBoolean;
		yourself
]

{ #category : #accessing }
RBFindAndReplaceRefactoring >> argumentsOf: aDictionary [
	|args limit|
	limit := self method ast arguments size - 1.
	args := OrderedCollection new.
	0 to: limit do: [ :each |
		args add: 
			(aDictionary at: (aDictionary keys detect: 
				[ :e | (e name asString) =  ('`@argMatch', each asString)])) sourceCode
	 ].
	^ args
]

{ #category : #accessing }
RBFindAndReplaceRefactoring >> copyOptionsFrom: aDictionary [ 
	| dict |
	dict := self options.
	dict == self class refactoringOptions 
		ifTrue: [^self options: aDictionary copy].
	dict keysAndValuesDo: 
			[:key :value | 
			value == (self class refactoringOptions  at: key ifAbsent: [nil]) 
				ifTrue: [
					
					dict at: key put: (aDictionary at: key)]].
	(aDictionary keys difference: dict keys) do: 
		[ :e | dict at: e put: (aDictionary at: e) ].
	self options: dict
]

{ #category : #accessing }
RBFindAndReplaceRefactoring >> extract: ocurrence of: aMethod [
	[|refactoring |
	refactoring := self extractMethodRefactoring. 
	refactoring model: self model.
	refactoring 	extract: ocurrence key from: aMethod selector in: aMethod modelClass.
	refactoring setOption: #existingSelector toUse:  [ :ref | 
			ref parameters: (self argumentsOf: ocurrence value).
			self method selector].
	self performComponentRefactoring: refactoring ] on: Exception do: [ :e | e ]
]

{ #category : #accessing }
RBFindAndReplaceRefactoring >> extractMethodRefactoring [
	^ RBExtractMethodRefactoring new
			setOption: #useExistingMethod 
			toUse:  [ :ref :aSelector | 
				true];
			yourself 
]

{ #category : #initialization }
RBFindAndReplaceRefactoring >> find: aSelector of: aClass inAllHierarchy: aBoolean [
	class := self classObjectFor: aClass.
	selector := aSelector.
	replacesAllHierarchy := aBoolean.
]

{ #category : #accessing }
RBFindAndReplaceRefactoring >> findAndReplaceOcurrencesIn: aMethod [ 
	self findOcurrencesIn: aMethod
]

{ #category : #accessing }
RBFindAndReplaceRefactoring >> findOcurrencesIn: aMethod [
	|methodNode sourceCode flag |
	flag := false.
	methodNode := aMethod ast.
	sourceCode := methodNode sourceCode.
	(self nodesOf: methodNode) do: [ :each |
            each first < each last
                ifTrue: [ 
	self matchNodes do: [ :matchNode | matchNode
                      match: (RBPatternParser parseExpression: (sourceCode copyFrom: each first to: each last ))
                      onSuccess: [ :map |
	self extract: ((each first to: each last) -> map) 
	of: aMethod.
	flag := true. ]
                      onFailure: [  ] .
						flag ifTrue: [ self findOcurrencesIn: (aMethod modelClass methodFor: aMethod selector).
							^ self]]]]. 
	methodNode body nodesDo: [ :node |
		self matchNodes do: [ :matchNode | matchNode
                      match: node
                      onSuccess: [ :map |
	self extract: ((node start to: node stop) -> map) 
	of: aMethod.
	flag := true.]
                      onFailure: [  ] .
						flag ifTrue: [ self findOcurrencesIn: (aMethod modelClass methodFor: aMethod selector).
							^ self]]].
]

{ #category : #accessing }
RBFindAndReplaceRefactoring >> matchNodes [
	^ matchNodes ifNil: [ |visitor node sourceCode|
		visitor := RBMatchVisitor new.
		node := self method ast copy .
		node acceptVisitor: visitor.
		sourceCode := self replaceArgumentsByPattern: node newSource .
		sourceCode := sourceCode copyFrom: (self method ast body statements first start + visitor difference) to: sourceCode size.
		matchNodes := OrderedCollection new.
		matchNodes add: (RBPatternParser parseExpression: sourceCode).
		node lastIsReturn ifTrue: [  node hasMultipleReturns 
		ifFalse: [ sourceCode := sourceCode copyReplaceAll: '^' with:  ''.
		matchNodes add: (RBPatternParser parseExpression: sourceCode) ] ].
		matchNodes]
]

{ #category : #accessing }
RBFindAndReplaceRefactoring >> method [
	^ method ifNil: [ method := class methodFor: selector ]
]

{ #category : #accessing }
RBFindAndReplaceRefactoring >> nodesOf: methodNode [ 
	|visitor node|
	visitor := RBCombinatorVisitor new.
	node := methodNode copy .
	node acceptVisitor: visitor.
	^ visitor combinations 
]

{ #category : #preconditions }
RBFindAndReplaceRefactoring >> preconditions [ 

	| condition |
	condition := replacesAllHierarchy 
		ifFalse: [ RBCondition empty ] 
		ifTrue: [ class allSubclasses 
						inject: RBCondition empty
						into: [ :cond :aClass | 
							cond & (RBCondition definesSelector: selector in: aClass) not] ].
	^condition
]

{ #category : #accessing }
RBFindAndReplaceRefactoring >> replaceArgumentsByPattern: sourceCode [ 
	|newSource|
	
	newSource := sourceCode copyWithRegex: 'tempMatch*' matchesReplacedWith: '`@tempMatch' .
	newSource := newSource copyWithRegex: 'argMatch*' matchesReplacedWith: '`@argMatch'.
	
	^ newSource 
	
]

{ #category : #printing }
RBFindAndReplaceRefactoring >> storeOn: aStream [
]

{ #category : #preconditions }
RBFindAndReplaceRefactoring >> transform [
	|classes|
	
	classes :=replacesAllHierarchy ifFalse: [ { class } ] ifTrue: [ class withAllSubclasses ].
	classes do: [ :cls | (cls selectors copyWithout: selector) do: [ :sel | |aMethod|
		aMethod := cls methodFor: sel.
		self findAndReplaceOcurrencesIn: aMethod] ]
]
