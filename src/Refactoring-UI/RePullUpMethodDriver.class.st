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
		'notSupersendsSent',
		'notSuperclassDefiner'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
RePullUpMethodDriver class >> driverName [

	^ 'Pull up method(s)'
]

{ #category : 'initialization' }
RePullUpMethodDriver class >> model: model scopes: refactoringScopes pullUpMethods: methods [

	^ self new
		  model: model
		  scopes: refactoringScopes
		  pullUpMethods: methods
]

{ #category : 'ui - dialogs' }
RePullUpMethodDriver >> alertFailedPreconditions [

	self application newAlert
		title: 'Pull Up method failed: Required preconditions were not met';
		label:  self failedPreconditionsErrorString;
		acceptLabel: 'Accept';
		openDialog
]

{ #category : 'execution' }
RePullUpMethodDriver >> breakingChoices [

	| choices |
	choices := OrderedCollection new.
	notSharedVarRefs isFalse ifTrue: [ choices add: (RePushUpSharedVariableChoice new driver: self) ].
	notInstVarRefs isFalse ifTrue: [ choices add: (RePushUpInstanceVariableChoice new driver: self) ].
	choices add: (RePullUpMethodTransformationChoice new driver: self).
	notOverrides isFalse ifTrue: [
			choices add: (ReBrowseMethodOverridesChoice new
					 driver: self;
					 description: 'Browse overriden method(s)') ].
	notSuperclassDefiner isFalse ifTrue: [ choices add: (ReCopyDownSuperclassMethodChoice new driver: self) ].

	^ choices
]

{ #category : 'actions' }
RePullUpMethodDriver >> browseOverrides [

	| overrides |
	overrides := notOverrides violators collect: [ :sel | superclass methodNamed: sel ].
	(application toolNamed: #messageList) browse: overrides
]

{ #category : 'actions' }
RePullUpMethodDriver >> browseSuperSendsSent [

	| overrides |
	overrides := notSupersendsSent violators.

	methods do: [ :method |
			(Smalltalk tools toolNamed: #messageList)
				browse: (overrides collect: [ :ref | ref compiledMethod ])
				asImplementorsOf: method selector ]
]

{ #category : 'execution' }
RePullUpMethodDriver >> changes [
	"it should NOT be generate changes."
	^  refactoring privateTransform

]

{ #category : 'copying' }
RePullUpMethodDriver >> copyDownMethods [

	| superclassDefiner subclasses oldSource oldProtocol refactoring2 |
	refactoring selectors do: [ :aSelector |
			notSuperclassDefiner violators do: [ :violator |
					violator second = aSelector ifFalse: [ ^ self ].
					superclassDefiner := violator first.

					subclasses := refactoring targetSuperclass subclasses reject: [ :each | each directlyDefinesMethod: aSelector ].

					subclasses ifEmpty: [ ^ self ].

					oldSource := superclassDefiner sourceCodeFor: aSelector.
					oldProtocol := superclassDefiner protocolsFor: aSelector.
					(superclassDefiner parseTreeForSelector: aSelector) superMessages
						detect: [ :each | superclassDefiner directlyDefinesMethod: each ]
						ifFound: [
								refactoring refactoringError:
									('Cannot pull up <1s> since we must copy down the superclass method in <2p><n>to the other subclasses, and the superclass method sends a super message which is overriden.'
										 expandMacrosWith: aSelector
										 with: superclassDefiner) ].

					refactoring2 := RBExpandReferencedPoolsRefactoring
						                model: refactoring model
						                forMethod: (superclassDefiner parseTreeForSelector: aSelector)
						                fromClass: superclassDefiner
						                toClasses: subclasses.

					refactoring generateChangesFor: refactoring2.

					subclasses do: [ :each |
						refactoring generateChangesFor: (RBAddMethodTransformation sourceCode: oldSource in: each withProtocol: oldProtocol) ] ] ].
	notInstVarRefs check
		ifFalse: [ self pullUpReferencedInstVars ]
		ifTrue: [ self pullUpMethods ]
]

{ #category : 'execution' }
RePullUpMethodDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		  title: 'There are potential breaking changes!';
		  message: self labelBasedOnBreakingChanges;
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  yourself
		  
]

{ #category : 'execution' }
RePullUpMethodDriver >> gatherUserInput [

	self selectMethodsAndClass.
	methods ifNil: [ ^ false ].
	refactoring selectors: (methods collect: [ :each | each selector ]).
	refactoring superClass: superclass.
	^ true
]

{ #category : 'initialization' }
RePullUpMethodDriver >> hasFailedBehaviorPreservingPreconditions [

	notOverrides := refactoring preconditionNoOverrides.
	notInstVarRefs := refactoring preconditionNoReferencesToInstVars.
	notSharedVarRefs := refactoring preconditionNoReferencesToSharedVars.
	notSupersendsSent := refactoring preconditionNoSupersendsSent.
	notSupersendsReceived := refactoring preconditionNoSupersendsReceived.
	notSuperclassDefiner := refactoring preconditionNoSuperclassDefiner.
	^ refactoring failedBreakingChangePreconditions isNotEmpty 
]

{ #category : 'configuration' }
RePullUpMethodDriver >> instantiateRefactoring [

	^ RePullUpMethodRefactoring
       model: model
       pullUp: #()
       from: class name
       to: ''
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

{ #category : 'transforming' }
RePullUpMethodDriver >> pullUpMethods [

	refactoring privateTransform.
	self openPreviewWithChanges: refactoring changes
]

{ #category : 'actions' }
RePullUpMethodDriver >> pullUpReferencedInstVars [

	notInstVarRefs referencedInstanceVariables do: [ :instVar | "We add the pushUpVariable refactoring to the refactoring previous transformations." "should we not call privateTransform instead of generateChanges"
		refactoring pullUpVariable: instVar ].
	self pullUpMethods
]

{ #category : 'actions' }
RePullUpMethodDriver >> pullUpReferencedSharedVars [

	notSharedVarRefs referencedSharedVariables do: [ :sharedVar | "We add the pushUpVariable transformation to the refactoring previous transformations"
		refactoring pullUpSharedVariable: sharedVar ].
	self pullUpMethods 
]

{ #category : 'execution' }
RePullUpMethodDriver >> selectMethodsAndClass [
	| dialog classes |
	
	classes := class allSuperclasses 
		removeAllSuchThat: [ :each | each == Object or: [ each == ProtoObject ] ].
	
	dialog := (self methodsAndSuperclassSelectionPresenterClass newApplication: self application)
		title: 'There are potential breaking changes!'
			label: 'Methods to be pulled up'
			dropLabel: 'Pull up methods from ' , class name , ' to:'
			withItems: (class methods sort: #asString ascending) asOrderedCollection
			selecting: methods asOrderedCollection
			dropItems: classes
			acceptBlock: [ :selectedClass :selectedMethods |
				superclass := selectedClass.
				methods := selectedMethods ];
		openModal.			

	dialog isOk ifTrue: [ ^ self ].
	
	superclass := nil.
	methods := nil
]
