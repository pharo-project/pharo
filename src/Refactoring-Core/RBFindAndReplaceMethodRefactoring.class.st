"
I am a tool to find occurrences of a method in other methods.

You can select a method and find the occurrences in a search range of your choice, in case of finding occurrences you can change the block containing the occurrence with a call to the selected method.

This refactoring goes hand in hand with RBExtractMethodRefactoring when replacing the code.
"
Class {
	#name : #RBFindAndReplaceMethodRefactoring,
	#superclass : #RBRefactoring,
	#instVars : [
		'method',
		'listToFindMatches',
		'class',
		'sourceSelector',
		'matchNodes',
		'positionIndex',
		'replaceAll'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'as yet unclassified' }
RBFindAndReplaceMethodRefactoring class >> find: aMethod [ 
	^ self new 
		method: aMethod; 
		yourself 
]

{ #category : #'as yet unclassified' }
RBFindAndReplaceMethodRefactoring class >> find: aMethod andReplaceIn: selectorCollection [
	^ self new 
		method: aMethod; 
		listToFindMatches: selectorCollection;
		yourself 
]

{ #category : #'find-replace' }
RBFindAndReplaceMethodRefactoring >> argumentsOf: aDictionary [
	|args limit|
	limit := method argumentNames size - 1.
	args := OrderedCollection new.
	0 to: limit do: [ :each |
		args add: 
			(aDictionary at: (aDictionary keys detect: 
				[ :e | (e name asString) =  ('`@argMatch', each asString)])) sourceCode
	 ].
	^ args
]

{ #category : #preconditions }
RBFindAndReplaceMethodRefactoring >> checkIsAbstractMethod [
	method isAbstract ifTrue: [ self refactoringError: ('<1p> method is abstract' expandMacrosWith: sourceSelector ) ]
]

{ #category : #preconditions }
RBFindAndReplaceMethodRefactoring >> checkIsSubclass: aClass [
	aClass ~= class ifTrue: [ 
		(class subclasses includes: aClass) 
			ifFalse: [ self refactoringError: 
			('<1p> is not subclass of <2s>'
			expandMacrosWith: aClass name
						with: class name) ] ]
]

{ #category : #preconditions }
RBFindAndReplaceMethodRefactoring >> checkOverridesSelectorInClass: each [
	each ~= method origin ifTrue:
		[((self model classNamed: each name asSymbol) directlyDefinesMethod: method selector)
		ifTrue: [ self refactoringError: 
			('<1p> overrides the method <2s>'
			expandMacrosWith: each name
			with: method selector) ]]
]

{ #category : #'find-replace' }
RBFindAndReplaceMethodRefactoring >> extract: ocurrence of: aMethod executing: aSymbol [
	|refactoring association |
	refactoring := self extractMethodRefactoring. 
	association := ocurrence asOrderedCollection at: positionIndex.
	refactoring 	extract: association key from: aMethod selector in: aMethod origin.
	refactoring setOption: #existingSelector toUse:  [ :ref | 
			ref parameters: (self argumentsOf: association value).
			method selector].
	refactoring perform: aSymbol.
	^ refactoring 
]

{ #category : #'find-replace' }
RBFindAndReplaceMethodRefactoring >> extractMethodRefactoring [
	^ RBExtractMethodRefactoring new
			setOption: #useExistingMethod 
			toUse:  [ :ref :selector | 
				UIManager default 
					confirm: 'Do you want use existing method ', selector printString, '?'
					label: 'Warninig'];
			yourself 
]

{ #category : #'find-replace' }
RBFindAndReplaceMethodRefactoring >> findAndReplaceOcurrencesIn: aMethod [ 
	|ocurrences|
	ocurrences := self findOcurrencesIn: aMethod selector of: aMethod origin. 
	ocurrences ifNotEmpty: [ 
		self replaceOcurrences: ocurrences on: aMethod
	]
]

{ #category : #'find-replace' }
RBFindAndReplaceMethodRefactoring >> findOcurrencesIn: aList [
	aList do: [ :aMethod | positionIndex := 1.
			 			[ self findAndReplaceOcurrencesIn: aMethod]
						on: Exception 
						do: [ :e | e messageText isEmptyOrNil
							ifFalse: [ UIManager default alert: e messageText ] ] 
				  ]
]

{ #category : #'find-replace' }
RBFindAndReplaceMethodRefactoring >> findOcurrencesIn: aMethod of: aClass [
	|methodNode matches sourceCode|
	methodNode := (aClass >> aMethod) sourceNode.
	sourceCode := methodNode sourceCode.
	matches := Set new.
	(self nodesOf: methodNode) do: [ :each | 
            each first < each last
                ifTrue: [ self match: (RBPatternParser parseExpression: (sourceCode copyFrom: each first to: each last )) 
										with: each first 
										and: each last 
										in: matches]].
	methodNode body nodesDo: [ :node | self match: node with: node start and: node stop in: matches ].
	
    ^ matches
]

{ #category : #'find-replace' }
RBFindAndReplaceMethodRefactoring >> findOcurrencesInClasses [
	listToFindMatches do: [ :aClass | 
		[(self preconditionsOf: aClass) check.
		  self findOcurrencesIn: aClass methods ]
						on: Exception 
						do: [ :e | e messageText isEmptyOrNil
							ifFalse: [ UIManager default alert: e messageText ] ] ]
]

{ #category : #accessing }
RBFindAndReplaceMethodRefactoring >> listToFindMatches: aCollection [
	listToFindMatches := aCollection 
]

{ #category : #'find-replace' }
RBFindAndReplaceMethodRefactoring >> match: node with: first and: last in: matches [  
	self matchNodes do: [ :matchNode | [ matchNode
                      match: node
                      onSuccess: [ :map | matches add: (first to: last) -> map ]
                      onFailure: [  ] ]
						on: Exception 
						do: [ :e | e ]]
]

{ #category : #initialization }
RBFindAndReplaceMethodRefactoring >> matchNodes [
	^ matchNodes ifNil: [ |visitor node sourceCode|
	
		visitor := RBMatchVisitor new.
		node := method sourceNode copy .
		node acceptVisitor: visitor.
		sourceCode := self replaceArgumentsByPattern: node newSource .
		sourceCode := sourceCode copyFrom: (method sourceNode body statements first start + visitor difference) to: sourceCode size.
		matchNodes := OrderedCollection new.
		matchNodes add: (RBPatternParser parseExpression: sourceCode).
		node lastIsReturn ifTrue: [  node hasMultipleReturns 
		ifFalse: [ sourceCode := sourceCode copyReplaceAll: '^' with:  ''.
		matchNodes add: (RBPatternParser parseExpression: sourceCode) ] ].
		matchNodes]
	
]

{ #category : #accessing }
RBFindAndReplaceMethodRefactoring >> method: aMethod [
	method := aMethod .
	class := method origin
]

{ #category : #'find-replace' }
RBFindAndReplaceMethodRefactoring >> nodesOf: methodNode [ 
	|visitor node|
	visitor := RBCombinatorVisitor new.
	node := methodNode copy .
	node acceptVisitor: visitor.
	^ visitor combinations 
]

{ #category : #'find-replace' }
RBFindAndReplaceMethodRefactoring >> orderOcurrences: ocurrences [
	^ ocurrences asOrderedCollection sort: [ :first :second | first key first < second key first ]
]

{ #category : #preconditions }
RBFindAndReplaceMethodRefactoring >> preconditionsOf: aClass [
	^ (RBCondition withBlock: [self checkIsSubclass: aClass. true])
		& (RBCondition withBlock: [self checkIsAbstractMethod. true])
		& (RBCondition withBlock: [self checkOverridesSelectorInClass: aClass . true ])
]

{ #category : #initialization }
RBFindAndReplaceMethodRefactoring >> replaceArgumentsByPattern: sourceCode [ 
	|newSource|
	
	newSource := sourceCode copyWithRegex: 'tempMatch*' matchesReplacedWith: '`@tempMatch' .
	newSource := newSource copyWithRegex: 'argMatch*' matchesReplacedWith: '`@argMatch'.
	
	^ newSource 
	
]

{ #category : #'find-replace' }
RBFindAndReplaceMethodRefactoring >> replaceOcurrences: ocurrences on: aMethod [
	
	ocurrences ifNotEmpty: [
		| refactoring dialog ocurrencesSize |
		ocurrencesSize := ocurrences size.
		replaceAll ifTrue: [ self extract: ( self orderOcurrences: ocurrences ) of: aMethod executing: #execute.
			ocurrencesSize := ocurrencesSize - 1 ]
		ifFalse: [ 
			refactoring := self extract: ( self orderOcurrences: ocurrences ) of: aMethod executing: #primitiveExecute.
			dialog := (RBReplacePreview change: refactoring) 
			cancelAction: [ :presenter | listToFindMatches := Array new. ocurrencesSize := -1.
				presenter beCancel; close ];
			skipAction: [ :presenter | positionIndex := positionIndex + 1.
				ocurrencesSize := ocurrencesSize + 1. presenter beOk; close ];
			replaceAllAction: [ :presenter | replaceAll := true. presenter beOk; close ]; open.
			dialog cancelled ifFalse: [ ocurrencesSize := ocurrencesSize - 1 ].
		 ].
	ocurrencesSize >= positionIndex ifTrue: [ self findAndReplaceOcurrencesIn: aMethod ] ]
]

{ #category : #transforming }
RBFindAndReplaceMethodRefactoring >> transform [
	replaceAll := false.
	listToFindMatches ifNil: [ (self options at: #listToSearch) value: method value: self  ].
	listToFindMatches first isClass ifTrue: [ self findOcurrencesInClasses ]
	ifFalse: [ self findOcurrencesIn: listToFindMatches ]
]
