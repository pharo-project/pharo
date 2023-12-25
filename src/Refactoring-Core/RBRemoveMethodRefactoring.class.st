"
I am a refactoring for removing a method.

My preconditions verify that this method is not referenced anywhere.
"
Class {
	#name : 'RBRemoveMethodRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'selector',
		'transformation'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBRemoveMethodRefactoring class >> model: aRBNamespace selector: aSelector from: aClass [

	^ self new
		  model: aRBNamespace;
		  selector: aSelector from: aClass;
		  yourself
]

{ #category : 'instance creation' }
RBRemoveMethodRefactoring class >> selector: aSelector from: aClass [

	^ self new
		selector: aSelector from: aClass
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> breakingChangePreconditions [

	^ (RBCondition withBlock: [
		   self checkSuperMethods.
		   true ]) & (RBCondition withBlock: [ self senders isEmpty ] errorString: 'Cannot remove method because it has senders')
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> checkBrowseAllOccurrences: anCollectionOfOccurrences [
	| methods callers |
	methods := anCollectionOfOccurrences collect: [ :c | c key ] as: Set.
	callers := anCollectionOfOccurrences collect: [ :v | v value ].
	methods size = 1
		ifTrue: [
			self
				refactoringWarning:
					('Possible call to <2s> in <1p> methods.<n>Browse references?'
						expandMacrosWith: anCollectionOfOccurrences size
						with: methods anyOne)
				with: [ self openBrowserOn: (RBBrowserEnvironment new referencesTo: methods anyOne) ] ]
		ifFalse: [
			self
				refactoringWarning:
					('Possible call to the <2p> selectors in <1p> methods.<n>Browse references?'
						expandMacrosWith: callers size
						with: methods size)
				with: [
					| env |
					env := RBSelectorEnvironment new.
					callers do: [ :d | env addMethod: d method ].
					self openBrowserOn: env ] ]
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> checkBrowseOccurrenceOf: aSelector in: aRBMethod [
	self
		refactoringWarning:
			('Possible call to <2s> in <1p><n>Browse references?' expandMacrosWith: aRBMethod modelClass with: aSelector)
		with: [ self openBrowserOn: (RBBrowserEnvironment new referencesTo: aSelector) ]
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> checkBrowseOccurrences: anCollectionOfOccurrences [
	^ anCollectionOfOccurrences size = 1
		ifTrue: [ self checkBrowseOccurrenceOf: anCollectionOfOccurrences first key in: anCollectionOfOccurrences first value ]
		ifFalse: [ self checkBrowseAllOccurrences: anCollectionOfOccurrences ]
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> checkPreconditions [ 

	transformation checkPreconditions.
	super checkPreconditions 
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> checkReferencesToAnyOf: aSelector [

	| occurrences |
	occurrences := self sendersOf: aSelector.
	occurrences ifEmpty: [ ^ self ].
	^ self checkBrowseOccurrences: occurrences
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> checkReferencesToSuperSendsToAnyOf: superMessages [
	[superMessages isEmpty] whileFalse:
			[self
				refactoringWarning: ('Although <1s> is equivalent to a superclass method,<n>it contains a super send so it might modify behavior.<n>Proceed anyway?'
						expandMacrosWith: superMessages first).
			superMessages remove: superMessages first]
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> checkSuperMethods [


	(self justSendsSuper: selector) ifFalse: [ ^ self ].
	
	(self superclassEquivalentlyDefines: selector)
		ifTrue:
			[(class parseTreeForSelector: selector) superMessages ifNotEmpty:
					[self checkReferencesToSuperSendsToAnyOf: selector]]

		ifFalse: [self checkReferencesToAnyOf: selector].
]

{ #category : 'executing' }
RBRemoveMethodRefactoring >> generateChanges [
	"Prepare, check the preconditions, and perform the actual transformation (i.e., creating a list of changes that can be accessible using the changes message). This method should not be invoked from drivers since drivers usually check in finer grained mode the preconditions - and that this stage they already checked the preconditions."
	
	self prepareForExecution.

	transformation applicabilityPreconditions check ifFalse: [
		^ RBApplicabilityChecksFailedError signal:
			  transformation applicabilityPreconditions errorString ].
	self breakingChangePreconditions check ifFalse: [
		RBBreakingChangeChecksFailedWarning signal:
			self breakingChangePreconditions errorString ].
	self flag: #todo. 
	"see other generateChanges"
	^ self removeMethodChanges 
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> justSendsSuper: aSelector [

	| matcher parseTree superclass |
	matcher := self parseTreeSearcherClass justSendsSuper.
	parseTree := class parseTreeForSelector: aSelector.
	( matcher executeTree: parseTree initialAnswer: false )
		ifFalse: [ ^ false ].
	parseTree lastIsReturn
		ifTrue: [ ^ true ].
	superclass := class superclass whichClassIncludesSelector: aSelector.
	superclass ifNil: [ ^ true ].	"Since there isn't a superclass that implements the message, we can
								 delete it since it would be an error anyway."
	parseTree := superclass parseTreeForSelector: aSelector.
	matcher := self parseTreeSearcher.
	matcher
		matches: '^``@object'
		do: [ :aNode :answer |
			answer
				add: aNode value;
				yourself
			].
	matcher executeTree: parseTree initialAnswer: Set new.
	^ ( matcher answer anySatisfy: [ :each | each isSelfVariable not ]) not
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> preconditions [

	^ self breakingChangePreconditions 
]

{ #category : 'transforming' }
RBRemoveMethodRefactoring >> privateTransform [
	 
	transformation privateTransform
]

{ #category : 'actions' }
RBRemoveMethodRefactoring >> removeMethodChanges [
	"Pay attention, don't call directly since this method skips preconditions. This should only be called by drivers when the preconditions have been met."

	self privateTransform.
	^ self changes
]

{ #category : 'accessing' }
RBRemoveMethodRefactoring >> selector [
	^ selector
]

{ #category : 'initialization' }
RBRemoveMethodRefactoring >> selector: aSelector from: aClass [

	class := self classObjectFor: aClass.
	selector := aSelector.
	transformation := RBRemoveMethodTransformation
		                  model: self model
		                  selector: selector
		                  from: aClass
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> senders [

	^ self sendersExcluding: { selector }
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> sendersExcluding: aSelectorsCollection [

	| occurrences |
	occurrences := OrderedCollection new.
	self 
		model allReferencesTo: selector
		do: [ :aRBMethod | 
			(aSelectorsCollection includes: aRBMethod selector) ifFalse: [
				occurrences add: selector -> aRBMethod ]].
	^ occurrences
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> sendersOf: aSelector [
	
	^ self model allReferencesTo: aSelector
]

{ #category : 'printing' }
RBRemoveMethodRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' removeMethods: '.
	selector storeOn: aStream.
	aStream nextPutAll: ' from: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'preconditions' }
RBRemoveMethodRefactoring >> superclassEquivalentlyDefines: aSelector [

	| superTree myTree |
	class superclass ifNil: [ ^ false ].
	superTree := class superclass parseTreeForSelector: aSelector.
	myTree := class parseTreeForSelector: aSelector.
	( superTree isNil or: [ myTree isNil ] )
		ifTrue: [ ^ false ].
	^ superTree equalTo: myTree exceptForVariables: #()
]
