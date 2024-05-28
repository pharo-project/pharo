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
	#name : 'RePushUpMethodDriver',
	#superclass : 'RBInteractionDriver',
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
RePushUpMethodDriver class >> model: model scopes: refactoringScopes pushUpMethods: methods [

	^ self new
		  model: model
		  scopes: refactoringScopes
		  pushUpMethods: methods
]

{ #category : 'execution' }
RePushUpMethodDriver >> breakingChoices [
	| choices |
	choices := OrderedCollection new.
	notOverrides check ifFalse:[ choices add: (ReBrowseMethodOverridesChoice new driver: self) ].
	notInstVarRefs  check ifFalse: [ choices add: (RePushUpInstanceVariableChoice new driver: self) ].
	notSharedVarRefs  check ifFalse: [ choices add: (RePushUpSharedVariableChoice new driver: self) ].
	notSupersendsReceived check ifFalse: [ choices add: (ReBrowseSubclassSuperSendsChoice new driver: self)  ].
	notSupersendsSent check ifFalse: [ self halt  ].
	
	^ choices
	
]

{ #category : 'actions' }
RePushUpMethodDriver >> browseOverrides [

	| overrides |
	overrides := notOverrides violators.
	methods do: [:method | 
		StMessageBrowser
		browse: (overrides collect: [ :ref | ref realClass methodNamed: method selector ])
		asImplementorsOf: method selector ]
	
]

{ #category : 'execution' }
RePushUpMethodDriver >> changes [
	"it should NOT be generate changes."
	^  refactoring privateTransform

]

{ #category : 'execution' }
RePushUpMethodDriver >> configureAndRunRefactoring [
	
	self configureRefactoring.

	refactoring failedApplicabilityPreconditions ifNotEmpty: [
		^ self inform: 'ToDo: Specify error' ].
	self setBreakingChangesPreconditions.
	refactoring failedBreakingChangePreconditions
		ifEmpty: [ self applyChanges ]
		ifNotEmpty: [ self handleBreakingChanges ]
]

{ #category : 'resources' }
RePushUpMethodDriver >> configureRefactoring [

	refactoring := RBPullUpMethodRefactoring
		               model: model
		               pullUp: (methods collect: [ :each | each selector ])
		               from: class name
		               to: superclass name
]

{ #category : 'configuration' }
RePushUpMethodDriver >> defaultSelectDialog [

	^ SpSelectDialog new
		    title: 'There are potential breaking changes!';
		          label: self labelBasedOnBreakingChanges;
		          items: self breakingChoices;
		          display: [ :each | each description ];
		          displayIcon: [ :each | self iconNamed: each systemIconName ];
		          openModal
]

{ #category : 'execution' }
RePushUpMethodDriver >> gatherUserInput [
	self selectMethodsAndClass
]

{ #category : 'execution' }
RePushUpMethodDriver >> handleBreakingChanges [

	| breakingConditions presenter changes |

	breakingConditions := {
		                      notOverrides.
		                      notInstVarRefs.
		                      notSharedVarRefs.
		                      notSupersendsSent.
		                      notSupersendsReceived } reject: [ :cond |
		                      cond check ].
	
	changes := breakingConditions collect: [:each | each class fixChoiceClass new driver: self ].
	presenter := self application newSelectMultiple.
	
	presenter
		title: 'There are potential breaking changes';
		label: self labelBasedOnBreakingChanges;
		items: changes;
		display: [ :each | each description ];
		onAccept: [ :dialog | | selectedChanges |
			selectedChanges := dialog presenter selectedItems.
			selectedChanges do: #action.
			selectedChanges ifNotEmpty: [ self applyChanges ] ];
		openDialog
]

{ #category : 'ui - dialogs' }
RePushUpMethodDriver >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream |
		  { notOverrides. notInstVarRefs . notSharedVarRefs . notSupersendsSent. notSupersendsReceived } 
			do: [ :cond |
			  cond check ifFalse: [
				  cond violationMessageOn: stream.
				  stream cr ] ].
		  stream nextPutAll: 'Select the strategies to apply' ]
]

{ #category : 'for mocks' }
RePushUpMethodDriver >> methodsAndSuperclassSelectionPresenterClass [

	^ methodSelectionPresenterClass ifNil: [ methodSelectionPresenterClass := StSelectClassAndMethodsPresenter ]
]

{ #category : 'for mocks' }
RePushUpMethodDriver >> methodsSelectionPresenterClass: aClass [ 

	methodSelectionPresenterClass := aClass
]

{ #category : 'initialization' }
RePushUpMethodDriver >> model: aRBNamespace scopes: refactoringScopes pushUpMethods: methodsList [ 
	
	model := aRBNamespace.
	scopes := refactoringScopes.
	methods := methodsList.
	class := methods first origin
]

{ #category : 'as yet unclassified' }
RePushUpMethodDriver >> pushUpReferencedInstVars [

	notInstVarRefs violators do: [ :violator | 
		"We add the pushUpVariable transformation to the refactoring previous transformations"
		refactoring pushUpVariable: (violator at: 2) ]
]

{ #category : 'execution' }
RePushUpMethodDriver >> runRefactoring [

	"the user can still select if needed. Nil = cancel refactoring"
	self gatherUserInput.
	methods ifNil: [ ^ self ].
	
	self configureAndRunRefactoring
]

{ #category : 'execution' }
RePushUpMethodDriver >> selectMethodsAndClass [

	| dialog classes |
	classes := class allSuperclasses removeAllSuchThat: [ :each |
		           each == Object or: [ each == ProtoObject ] ].
	dialog := self methodsAndSuperclassSelectionPresenterClass
		          label: 'Methods to be pull up'
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
RePushUpMethodDriver >> setBreakingChangesPreconditions [
	notOverrides := refactoring preconditionNoOverrides.
	notInstVarRefs := refactoring preconditionNoReferencesToInstVars.
	notSharedVarRefs := refactoring preconditionNoReferencesToSharedVars.
	notSupersendsSent := refactoring preconditionNoSupersendsSent.
	notSupersendsReceived := refactoring preconditionNoSupersendsReceived
]
