"
I am the abstract base class for refactoring operations. 

I define the common workflow for a refactoring:
check precondition, 
primitive execute - a dry run collecting the changes without applying them,
and execute - run and apply changes.

I provide many utility methods used by my subclasses. 
Every  concrete subclass implements a single refactoring. They have to implement the methods
preconditions and transform.


Instance Variables

options:
Some refactorings may need user interactions or some extra data for performing
the operation, the code for requesting this data is stored in a block associated with a ""refacotring option""
(see RBRefactoring>>#setOption:toUse:  and RBRefactoring class>>#initializeRefactoringOptions).

model:
My model - a RBNamespace - defines the environment in which my refactoring is applied and collects all changes (RBRefactoryChange).

A RBRefactoringManager  is used to collect the executed refactorings and provides an undo and redo facility.

"
Class {
	#name : 'ReRefactoring',
	#superclass : 'ReAbstractTransformation',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'testing' }
ReRefactoring class >> isAbstract [

	^ self == ReRefactoring
]

{ #category : 'accessing signal' }
ReRefactoring class >> preconditionSignal [
	^ RBRefactoringError , RBRefactoringWarning
]

{ #category : 'converting' }
ReRefactoring >> asRefactoring [

	self deprecated: 'This method is a no-op and can be removed' transformWith: '`@rec asRefactoring' -> '`@rec'.
	^ self
]

{ #category : 'preconditions' }
ReRefactoring >> breakingChangePreconditions [

	^ #(  )
]

{ #category : 'testing' }
ReRefactoring >> canReferenceVariable: aString in: aClass [
	(aClass definesVariable: aString) ifTrue: [^true].
	(self model includesGlobal: aString asSymbol) ifTrue: [^true].
	^(self poolVariableNamesFor: aClass) includes: aString
]

{ #category : 'scripting api - conditions' }
ReRefactoring >> checkBreakingChangePreconditions [
	"Check a preconditions and raise an error on violations. This method is part of the scripting API since it raises an error."
	
	| failedPreconditions |
	failedPreconditions := self failedBreakingChangePreconditions.
	failedPreconditions ifEmpty: [ ^ self ].
	RBRefactoringWarning signalFor: failedPreconditions
]

{ #category : 'private' }
ReRefactoring >> checkClass: aRBClass selector: aSelector using: aMatcher [
	| parseTree |
	parseTree := aRBClass parseTreeForSelector: aSelector.
	parseTree ifNotNil: [aMatcher executeTree: parseTree].
	^aMatcher answer
]

{ #category : 'scripting api - conditions' }
ReRefactoring >> checkPreconditions [ 

	self checkApplicabilityPreconditions.
	self checkBreakingChangePreconditions 
]

{ #category : 'private' }
ReRefactoring >> classObjectFor: anObject [

	(anObject isBehavior or: [ anObject isTrait ]) ifTrue: [
		^ self model classFor: anObject ].
	anObject isString ifTrue: [
		^ self model classNamed: anObject ].
	^ anObject
]

{ #category : 'private' }
ReRefactoring >> convertAllReferencesToClass: aRBClass using: searchReplacer [
	self model allReferencesToClass: aRBClass
		do:
			[:method |
			self
				convertMethod: method selector
				for: method modelClass
				using: searchReplacer]
]

{ #category : 'private' }
ReRefactoring >> convertClasses: classSet select: aBlock using: searchReplacer [
	classSet do:
			[:aClass |
			(aBlock value: aClass) do:
					[:selector |
					self
						convertMethod: selector
						for: aClass
						using: searchReplacer]]
]

{ #category : 'preconditions' }
ReRefactoring >> failedBreakingChangePreconditions [
	"Returne the failed preconditions without raising error. It should only be called by drivers."
	
	^ self breakingChangePreconditions reject: [ :cond | cond check ]
]

{ #category : 'private' }
ReRefactoring >> onError: aBlock do: errorBlock [
	^aBlock on: self class preconditionSignal
		do:
			[:ex |
			errorBlock cull: ex.
			ex return: nil]
]

{ #category : 'removing' }
ReRefactoring >> removeReturnsOf: parseTree [
	| rewriter |
	rewriter := self parseTreeRewriter.
	rewriter replace: '^``@object' with: '``@object'.
	(rewriter executeTree: parseTree)
		ifTrue: [ ^ rewriter tree].
	^ parseTree
]

{ #category : 'private' }
ReRefactoring >> safeVariableNameFor: aClass temporaries: allTempAndArgs basedOn: aString [
	| baseString i newString |
	newString := baseString := aString.
	i := 0.
	[ (allTempAndArgs includes: newString) or: [ aClass definesInstanceVariable: newString ] ]
		whileTrue: [
			i := i + 1.
			newString := baseString , i printString ].
	^ newString
]

{ #category : 'transforming' }
ReRefactoring >> transform [ 

	self deprecated: 'Use privateTransform instead' transformWith: '`@rec transform' -> '`@rec privateTransform'
]

{ #category : 'utilities' }
ReRefactoring >> whichVariableNode: aParseTree inInterval: anInterval name: aName [
	| matcher block |
	matcher := self parseTreeSearcher.
	block := [ :aNode :answer |
	(aNode intersectsInterval: anInterval)
		ifTrue: [ aNode ]
		ifFalse: [ answer ] ].
	matcher
		matches: aName do: block;
		matchesArgument: aName do: block.
	^ matcher executeTree: aParseTree initialAnswer: nil
]
