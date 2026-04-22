"
I represent a bridge between refactoring core and UI. I am abstract class and you should use my subclasses to invoke specific refactorings.

I am a driver object responsible for invoking refactorings. I am responsible for:
- gathering information from user that are needed to perform a refactoring
- checking preconditions and raising an error if they are not fulfilled
- checking braking changes and warning user if they exist
- and finally, invoking refactoring


"
Class {
	#name : 'ReInteractionDriver',
	#superclass : 'Object',
	#instVars : [
		'application',
		'model',
		'scopes',
		'refactoring',
		'previewPresenterClass',
		'requestDialog',
		'informDialog',
		'stoppedBeforeApplyingRefactoring',
		'selectDialogForBreakingChanges',
		'forTesting'
	],
	#classInstVars : [
		'shouldPreviewChanges'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReInteractionDriver class >> driverName [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReInteractionDriver class >> drivers [

	^ self allSubclasses reject: [ :subclass | subclass isAbstract ]
]

{ #category : 'testing' }
ReInteractionDriver class >> isAbstract [

	^ self == ReInteractionDriver
]

{ #category : 'displaying' }
ReInteractionDriver class >> menuItemString [
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self driverName
]

{ #category : 'instance creation' }
ReInteractionDriver class >> newApplication: anApplication [

	^ self new 
		application: anApplication;
		yourself
]

{ #category : 'settings' }
ReInteractionDriver class >> refactoringsSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: #Refactorings)
		label: 'Refactorings';
		description: 'Settings about the refactorings and transformations of Pharo.';
		parent: #tools;
		with: [
				(aBuilder group: #Previews)
					label: 'Preview changes';
					description: 'A list of settings to declare for which refactoring the user wants a preview of the cnahges before applying them or not.';
					with: [
							self drivers do: [ :aDriver |
										(aBuilder setting: aDriver name asSymbol)
											label: 'Preview changes: ' , aDriver menuItemString;
											selector: #shouldPreviewChanges;
											default: self shouldPreviewChangesByDefault;
											target: aDriver ] ] ]
]

{ #category : 'accessing' }
ReInteractionDriver class >> shouldPreviewChanges [

	^ shouldPreviewChanges ifNil: [ shouldPreviewChanges := self shouldPreviewChangesByDefault ]
]

{ #category : 'accessing' }
ReInteractionDriver class >> shouldPreviewChanges: anObject [

	shouldPreviewChanges := anObject
]

{ #category : 'accessing' }
ReInteractionDriver class >> shouldPreviewChangesByDefault [

	^ true
]

{ #category : 'accessing' }
ReInteractionDriver >> application [
	"For now use the singleton. Normally the application should be passed from Calypso and the command"
	
	^ application ifNil: [ StPharoApplication current ]
]

{ #category : 'accessing' }
ReInteractionDriver >> application: anApplication [
	application := anApplication
]

{ #category : 'execution' }
ReInteractionDriver >> applyChanges [

	| applied |

	forTesting ifTrue: [ ^ self ].
	applied := self openPreviewWithChanges: self changes "it looks like there is a bug in Spec" "stoppedBeforeApplyingRefactoring := applied isCancelled not
	isCancelled returns always true so I will use it when this will be fixed in spec."
]

{ #category : 'execution' }
ReInteractionDriver >> changes [
	"Drivers should not call generateChanges of Refactoring because generateChanges is performing the preconditions and raising errors 
	The API method generateChanges is for refactoring scripting."
	
	refactoring privateTransform.
	^ refactoring changes

]

{ #category : 'configuration' }
ReInteractionDriver >> configureRefactoring [

	refactoring := self instantiateRefactoring.
	refactoring prepareForExecution
]

{ #category : 'ui - requests' }
ReInteractionDriver >> confirm: aString [

	^ self application confirm: aString
]

{ #category : 'private' }
ReInteractionDriver >> defaultInformDialog [

  ^ self application newInform
]

{ #category : 'configuration' }
ReInteractionDriver >> defaultRequestDialog [
	"We do not use a subclassResponsibility because all the drivers do not have to."

	^ self application newRequest
]

{ #category : 'configuration' }
ReInteractionDriver >> defaultSelectDialog [
	"We do not use a subclassResponsibility because all the drivers do not have to."
	
	^ self application newSelect
]

{ #category : 'ui - dialogs' }
ReInteractionDriver >> failedPreconditionsErrorString [

	^ String streamContents: [ :stream |
		  refactoring failedApplicabilityPreconditions do: [ :cond |
				  cond violationMessageOn: stream.
				  stream cr ] ]
]

{ #category : 'ui - dialogs' }
ReInteractionDriver >> failedPreconditionsErrorString: conditions [

	^ String streamContents: [ :stream |
		  conditions do: [ :cond |
				  cond violationMessageOn: stream.
				  stream cr ] ]
]

{ #category : 'accessing' }
ReInteractionDriver >> forTesting [

	^ forTesting
]

{ #category : 'accessing' }
ReInteractionDriver >> forTesting: aBoolean [

	forTesting := aBoolean
]

{ #category : 'execution' }
ReInteractionDriver >> furtherActionFor: aReport [
	
	aReport browse
]

{ #category : 'execution' }
ReInteractionDriver >> gatherUserInput [
	"In this hood driver gathers inputs from user.
	Return false if opration was unsuccessful (like user wants to abandon), and true otherwise"
	^ true
]

{ #category : 'execution' }
ReInteractionDriver >> handleBreakingChanges [

	| selection dialog |
	dialog := self selectDialogForBreakingChanges.
	selection := dialog openModal.
	selection ifNil: [ ^ false ].
	selection action.
	^ true
]

{ #category : 'private ex' }
ReInteractionDriver >> hasFailedApplicabilityPreconditions [ 
	
	refactoring failedApplicabilityPreconditions 
		ifNotEmpty: [ self inform: self failedPreconditionsErrorString. ^ true ].
	^ false
]

{ #category : 'private exe' }
ReInteractionDriver >> hasFailedBehaviorPreservingPreconditions [

	^ refactoring failedBreakingChangePreconditions isNotEmpty
]

{ #category : 'ui - requests' }
ReInteractionDriver >> inform: aString [
	"Display a message for the user to read and then dismiss."

	aString isEmptyOrNil ifTrue: [ ^ self ].
	self application inform: aString
]

{ #category : 'private' }
ReInteractionDriver >> informConditions: conditions [ 
	"The receiver has multiple failed applicability conditions and more than one could fail, inform all failures"

	conditions do: [ :cond | self inform: cond errorString ].
]

{ #category : 'private' }
ReInteractionDriver >> informDialog [
	"I'm hook to inject mock in tests. The driver should specialize defaultInformDialog"
	
	^ informDialog ifNil: [ informDialog := self defaultInformDialog ]
]

{ #category : 'private' }
ReInteractionDriver >> informDialog: aDialog [

	 informDialog := aDialog
]

{ #category : 'initialization' }
ReInteractionDriver >> initialize [

	super initialize.
	forTesting := false.
	stoppedBeforeApplyingRefactoring := false "for now unused but we should soon use it. Check applyChanges"
]

{ #category : 'configuration' }
ReInteractionDriver >> instantiateRefactoring [
	"I return a new refactoring instance, ready to run."
	self subclassResponsibility
]

{ #category : 'accessing' }
ReInteractionDriver >> model [
	"Answer the receiver's <RBBrowserEnvironment>"

	^ model
]

{ #category : 'accessing' }
ReInteractionDriver >> model: aRBBrowserEnvironment [ 
	model := aRBBrowserEnvironment
]

{ #category : 'execution' }
ReInteractionDriver >> openPreviewWithChanges: changes [
	| presenter |
	
	presenter := (self previewPresenterClass newApplication: self application)
		changesFrom: changes scopes: scopes;
		refactoring: self refactoring;
		yourself.

	self class shouldPreviewChanges
		ifTrue: [ presenter openModal ]
		ifFalse: [ presenter accept ]
]

{ #category : 'configuration' }
ReInteractionDriver >> previewPresenterClass [

	^ previewPresenterClass ifNil: [ previewPresenterClass := StRefactoringPreviewPresenter ] 
	
	
]

{ #category : 'configuration' }
ReInteractionDriver >> previewPresenterClass: aClass [

	 previewPresenterClass := aClass
	
	
]

{ #category : 'resources' }
ReInteractionDriver >> refactoring [
	^ refactoring
]

{ #category : 'utilities' }
ReInteractionDriver >> refactoringScopeOn: aScope [

	^ RBNamespace onEnvironment: aScope asRBEnvironment
]

{ #category : 'configuration' }
ReInteractionDriver >> requestDialog [
	
	^ requestDialog ifNil: [ requestDialog := self defaultRequestDialog ]
	
]

{ #category : 'configuration' }
ReInteractionDriver >> requestDialog: aDialog [
	
	requestDialog := aDialog

	
]

{ #category : 'execution' }
ReInteractionDriver >> runRefactoring [

	self configureRefactoring.
	self gatherUserInput ifFalse: [ ^ self "user abandons" ].
	self hasFailedApplicabilityPreconditions ifTrue: [ ^ self "applicability failed" ].
	self hasFailedBehaviorPreservingPreconditions
		ifTrue: [ self handleBreakingChanges ]
		ifFalse: [ self applyChanges ].

]

{ #category : 'accessing' }
ReInteractionDriver >> scopes [

	^ scopes
]

{ #category : 'accessing' }
ReInteractionDriver >> scopes: anObject [

	scopes := anObject
]

{ #category : 'configuration' }
ReInteractionDriver >> selectDialogForBreakingChanges [

	^ selectDialogForBreakingChanges ifNil: [ selectDialogForBreakingChanges := self defaultSelectDialog ].
	
]

{ #category : 'configuration' }
ReInteractionDriver >> selectDialogForBreakingChanges: aDialog [
	
	selectDialogForBreakingChanges := aDialog 
	
]
