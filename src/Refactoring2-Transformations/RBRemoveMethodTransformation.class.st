"
Removes a method from a class. Only the selector of the method is needed.

Usage:
| transformation |
transformation := (RBRemoveMethodTransformation 
		selector: #transform
		from: RBRemoveMethodTransformation)
		transform. 
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
Checks whether there are any references to this method
"
Class {
	#name : #RBRemoveMethodTransformation,
	#superclass : #RBMethodTransformation,
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBRemoveMethodTransformation class >> model: aRBModel selector: aSelector from: aClassName [

	^ self new
		model: aRBModel;
		selector: aSelector
		from: aClassName;
		yourself
]

{ #category : #api }
RBRemoveMethodTransformation class >> model: aRBModel selectors: aCollection from: aClassName [

	^ RBCompositeTransformation new
		transformations: (aCollection collect: [:e |
			self model: aRBModel
				  selector: e
				  from: aClassName] )
]

{ #category : #api }
RBRemoveMethodTransformation class >> selector: aSelector from: aClassName [

	^ self new
		selector: aSelector
		from: aClassName;
		yourself
]

{ #category : #api }
RBRemoveMethodTransformation class >> selectors: aCollection from: aClassName [

	^ self model: RBTransformation new model selectors: aCollection from: aClassName
]

{ #category : #preconditions }
RBRemoveMethodTransformation >> checkBrowseAllOccurences: anCollectionOfOccurrences [

	| methods callers |
	methods := anCollectionOfOccurrences collect: [ :c | c key ] as: Set.
	callers := anCollectionOfOccurrences collect: [ :v | v value ].
	methods size = 1
		ifTrue: [ self refactoringError:
				('Possible call to <2s> in <1p> methods.<n>Browse references?'
					expandMacrosWith: anCollectionOfOccurrences size
					with: methods anyOne)
				with: [ self openBrowserOn: (RBBrowserEnvironment new referencesTo: methods anyOne) ] ]
		ifFalse: [ self refactoringError:
				('Possible call to the <2p> selectors in <1p> methods.<n>Browse references?'
					expandMacrosWith: callers size
					with: methods size)
				with: [ 
					| env |
					env := RBSelectorEnvironment new.
					callers do: [ :d | env addMethod: d method ].
					self openBrowserOn: env ] ]
]

{ #category : #preconditions }
RBRemoveMethodTransformation >> checkBrowseOccurrenceIn: aRBMethod [

	self refactoringError:
		('Possible call to <2s> in <1p><n>Browse references?'
			expandMacrosWith: aRBMethod modelClass
			with: selector)
		with: [ self openBrowserOn: (RBBrowserEnvironment new referencesTo: selector) ]
]

{ #category : #preconditions }
RBRemoveMethodTransformation >> checkBrowseOccurrences: anCollectionOfOccurrences [

	anCollectionOfOccurrences size = 1
		ifTrue: [ ^ self checkBrowseOccurrenceIn: anCollectionOfOccurrences first value ]
		ifFalse: [ ^ self checkBrowseAllOccurences: anCollectionOfOccurrences ]
]

{ #category : #preconditions }
RBRemoveMethodTransformation >> checkReferences [

	| occurrences |
	occurrences := OrderedCollection new.

	self model
		  allReferencesTo: selector
		  do: [ :aRBMethod | 
				(selector = aRBMethod selector)
				ifFalse: [ occurrences add: selector -> aRBMethod ] ].
	occurrences ifEmpty: [ ^ self ].
	^ self checkBrowseOccurrences: occurrences
]

{ #category : #preconditions }
RBRemoveMethodTransformation >> checkSuperMethods [

	(self justSendsSuper: selector) ifTrue: [ ^ self ]
		ifFalse: [ (self superclassEquivalentlyDefines: selector)
			ifTrue: [ (self definingClass parseTreeFor: selector) superMessages
				ifNotEmpty: [ self 
						refactoringError: ('Although <1s> is equivalent to a superclass method,<n>
							it contains a super send so it might modify behavior.'
							expandMacrosWith: selector ) ] ]
			ifFalse: [ self checkReferences ] ]
]

{ #category : #preconditions }
RBRemoveMethodTransformation >> justSendsSuper: aSelector [
	| matcher parseTree superclass |
	matcher := self parseTreeSearcherClass justSendsSuper.
	parseTree := self definingClass parseTreeFor: aSelector.
	(matcher executeTree: parseTree initialAnswer: false)
		ifFalse: [ ^ false ].
	parseTree lastIsReturn
		ifTrue: [ ^ true ].	"Since there isn't a superclass that implements the message, we can 
	 delete it since it would be an error anyway."
	superclass := self definingClass superclass
		whichClassIncludesSelector: aSelector.
	superclass ifNil: [ ^ true ].
	parseTree := superclass parseTreeFor: aSelector.
	matcher := self parseTreeSearcherClass new.
	matcher
		matches: '^``@object'
		do: [ :aNode :answer | 
			answer
				add: aNode value;
				yourself ].
	matcher executeTree: parseTree initialAnswer: Set new.
	^ matcher answer
		allSatisfy: [ :each | "^ (matcher answer 
		detect: [:each | (each isVariable and: [each name = 'self']) not]
		ifNone: [nil]) isNil" each isVariable and: [ each name = 'self' ] ]
]

{ #category : #preconditions }
RBRemoveMethodTransformation >> preconditions [

	^ (RBCondition definesSelector: selector in: self definingClass) 
		& (RBCondition withBlock: [ self checkSuperMethods. true ])
]

{ #category : #executing }
RBRemoveMethodTransformation >> privateTransform [

	self definingClass removeMethod: selector
]

{ #category : #api }
RBRemoveMethodTransformation >> selector: aSelector from: aClassName [

	self className: aClassName.
	selector := aSelector
	
]

{ #category : #printing }
RBRemoveMethodTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' selector: ';
		nextPutAll: selector;
		nextPutAll: ' from: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #preconditions }
RBRemoveMethodTransformation >> superclassEquivalentlyDefines: aSelector [

	| superTree myTree |

	self definingClass superclass ifNil: [ ^ false ].
	superTree := self definingClass superclass parseTreeFor: aSelector.
	myTree := self definingClass parseTreeFor: aSelector.
	( superTree isNil or: [ myTree isNil ] )
		ifTrue: [ ^ false ].
	^ superTree equalTo: myTree exceptForVariables: #()
]
