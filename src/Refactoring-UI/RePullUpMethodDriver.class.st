"
I represent a driver that invokes `PullUpMethod` refactoring.

I am responsible for asking the user to which classes the method will be pulled up.

When I gather all needed information I am calling and executing pull up method refactoring.

You can create my instance and execute the refactoring by running:

```
(RBPushUpMethodDriver model: aRBNamespace scopes: refactoringScopes pushDown: methods) runRefactoring
```
"
Class {
	#name : 'RePullUpMethodDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'methods',
		'class',
		'methodSelectionPresenterClass',
		'superclass',
		'notOverrides',
		'notInstVarRefs',
		'notSharedVarRefs',
		'notSupersendsReceived',
		'notSupersendsSent'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'initialization' }
RePullUpMethodDriver class >> model: model scopes: refactoringScopes pullUpMethods: methods [

	^ self new
		  model: model
		  scopes: refactoringScopes
		  pullUpMethods: methods
]

{ #category : 'ui - dialogs' }
RePullUpMethodDriver >> alertFailedPreconditions [
	SpAlertDialog new
		title: 'Pull Up method failed: Required preconditions were not met';
		label:  self failedPreconditionsErrorString;
		acceptLabel: 'Accept';
		openDialog
]

{ #category : 'execution' }
RePullUpMethodDriver >> breakingChoices [

	^ refactoring failedBreakingChangePreconditions collect: [ :each |
		each class strategyChoiceClass new driver: self ]
]

{ #category : 'actions' }
RePullUpMethodDriver >> browseOverrides [

	| overrides |
	overrides := notOverrides violators.
	methods do: [:method | 
		StMessageBrowser
		browse: (overrides collect: [ :ref | ref realClass methodNamed: method selector ])
		asImplementorsOf: method selector ]
	
]

{ #category : 'actions' }
RePullUpMethodDriver >> browseSuperSendsSent [
	| overrides |
	overrides := notSupersendsSent violators.
	
	methods do: [:method | 
		StMessageBrowser
		browse: (overrides collect: [ :ref | ref compiledMethod ])
		asImplementorsOf: method selector ]
	
]

{ #category : 'execution' }
RePullUpMethodDriver >> changes [
	"it should NOT be generate changes."
	^  refactoring privateTransform

]

{ #category : 'execution' }
RePullUpMethodDriver >> configureAndRunRefactoring [

	self configureRefactoring.

	refactoring failedApplicabilityPreconditions ifNotEmpty: [
		^ self alertFailedPreconditions ].
	
	self setBreakingChangesPreconditions.
	refactoring failedBreakingChangePreconditions
		ifEmpty: [ self applyChanges ]
		ifNotEmpty: [ self handleBreakingChanges ]
]

{ #category : 'resources' }
RePullUpMethodDriver >> configureRefactoring [

	refactoring := RePullUpMethodRefactoring
		               model: model
		               pullUp: (methods collect: [ :each | each selector ])
		               from: class name
		               to: superclass name
]

{ #category : 'execution' }
RePullUpMethodDriver >> gatherUserInput [
	self selectMethodsAndClass
]

{ #category : 'execution' }
RePullUpMethodDriver >> handleBreakingChanges [

	| presenter choices |
	
	"Display previous transformations that will fix the fixable breaking conditions"
	choices := self breakingChoices.
	
	
	presenter := self application newSelectMultiple.

	presenter
		title: 'There are potential breaking changes';
		label: self labelBasedOnBreakingChanges;
		items: choices;
		display: [ :each | each description ];
		onAccept: [ :dialog |
			| selectedChanges |
			selectedChanges := dialog presenter selectedItems.
			selectedChanges do: #action.
			self applyChanges ];
		openDialog
]

{ #category : 'ui - dialogs' }
RePullUpMethodDriver >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream |
		  refactoring failedBreakingChangePreconditions do: [ :cond |
			  cond check ifFalse: [
				  cond violationMessageOn: stream.
				  stream cr ] ].
		  stream nextPutAll: 'Select the strategies to apply' ]
]

{ #category : 'for mocks' }
RePullUpMethodDriver >> methodsAndSuperclassSelectionPresenterClass [

	^ methodSelectionPresenterClass ifNil: [ methodSelectionPresenterClass := StSelectClassAndMethodsPresenter ]
]

{ #category : 'for mocks' }
RePullUpMethodDriver >> methodsSelectionPresenterClass: aClass [ 

	methodSelectionPresenterClass := aClass
]

{ #category : 'initialization' }
RePullUpMethodDriver >> model: aRBNamespace scopes: refactoringScopes pullUpMethods: methodsList [ 
	
	model := aRBNamespace.
	scopes := refactoringScopes.
	methods := methodsList.
	class := methods first origin
]

{ #category : 'actions' }
RePullUpMethodDriver >> pullUpReferencedInstVars [

	notInstVarRefs referencedInstanceVariables do: [ :instVar | 
		"We add the pushUpVariable transformation to the refactoring previous transformations"
		refactoring pullUpVariable: instVar ]
]

{ #category : 'actions' }
RePullUpMethodDriver >> pullUpReferencedSharedVars [

	notSharedVarRefs referencedSharedVariables do: [ :sharedVar | 
		"We add the pushUpVariable transformation to the refactoring previous transformations"
		refactoring pullUpSharedVariable: sharedVar ]
]

{ #category : 'execution' }
RePullUpMethodDriver >> runRefactoring [

	"the user can still select if needed. Nil = cancel refactoring"
	self gatherUserInput.
	methods ifNil: [ ^ self ].
	self configureAndRunRefactoring
]

{ #category : 'execution' }
RePullUpMethodDriver >> selectMethodsAndClass [

	| dialog classes |
	classes := class allSuperclasses removeAllSuchThat: [ :each |
		           each == Object or: [ each == ProtoObject ] ].
	dialog := self methodsAndSuperclassSelectionPresenterClass
		          label: 'Methods to be pulled up'
		          dropLabel: 'Pull up methods from ' , class name , ' to:'
		          withItems:
			          (class methods sort: [ :a :b | a asString < b asString ])
				          asOrderedCollection
		          selecting: methods asOrderedCollection
		          dropItems: classes
		          acceptBlock: [ :selectedClass :selectedMethods |
			          superclass := selectedClass.
			          methods := selectedMethods ].

	dialog cancelled ifFalse: [ ^ self ].
	superclass := nil.
	methods := nil
]

{ #category : 'initialization' }
RePullUpMethodDriver >> setBreakingChangesPreconditions [

	notOverrides := refactoring preconditionNoOverrides.
	notInstVarRefs := refactoring preconditionNoReferencesToInstVars.
	notSharedVarRefs := refactoring preconditionNoReferencesToSharedVars.
	notSupersendsSent := refactoring preconditionNoSupersendsSent.
	notSupersendsReceived := refactoring preconditionNoSupersendsReceived.
	
]
