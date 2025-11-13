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
		'selectDialog',
		'requestDialog',
		'informDialog',
		'stoppedBeforeApplyingRefactoring'
	],
	#classVars : [
		'DefaultRequestDialog',
		'DefaultSelectDialog'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'configuration' }
ReInteractionDriver class >> defaultRequestDialog [ 

	^ DefaultRequestDialog ifNil: [ SpRequestDialog ]
]

{ #category : 'configuration' }
ReInteractionDriver class >> defaultSelectDialog [ 

	^ DefaultSelectDialog ifNil: [ SpSelectDialog ]
]

{ #category : 'testing' }
ReInteractionDriver class >> isAbstract [

	^ self == ReInteractionDriver
]

{ #category : 'displaying' }
ReInteractionDriver class >> menuItemString [
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self refactoringClass menuItemString
]

{ #category : 'instance creation' }
ReInteractionDriver class >> newApplication: anApplication [

	^ self new 
		application: anApplication;
		yourself
]

{ #category : 'displaying' }
ReInteractionDriver class >> refactoringClass [

	self subclassResponsibility
]

{ #category : 'configuration' }
ReInteractionDriver class >> useRequestDialog: aRequestDialog during: aBlock [

	| old |
	old := DefaultRequestDialog.
	DefaultRequestDialog := aRequestDialog.
	^ aBlock ensure: [ DefaultRequestDialog := old ]
]

{ #category : 'configuration' }
ReInteractionDriver class >> useSelectDialog: aSelectDialog during: aBlock [

	| old |
	old := DefaultSelectDialog.
	DefaultSelectDialog := aSelectDialog.
	^ aBlock ensure: [ DefaultSelectDialog := old ]
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
	applied := self openPreviewWithChanges: self changes.
	"it looks like there is a bug in Spec"
	"stoppedBeforeApplyingRefactoring := applied isCancelled not
	isCancelled returns always true so I will use it when this will be fixed in spec."
]

{ #category : 'execution' }
ReInteractionDriver >> changes [
	"Drivers should not call generateChanges of Refactoring because generateChanges is performing the preconditions and raising errors 
	The API method generateChanges is for refactoring scripting."
	
	refactoring privateTransform.
	^ refactoring changes

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

	^ self class defaultRequestDialog newApplication: self application
]

{ #category : 'configuration' }
ReInteractionDriver >> defaultSelectDialog [
	"We do not use a subclassResponsibility because all the drivers do not have to."
	
	^ self class defaultSelectDialog newApplication: self application
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

{ #category : 'execution' }
ReInteractionDriver >> furtherActionFor: aReport [
	
	aReport browse
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
	stoppedBeforeApplyingRefactoring := false.
	"for now unused but we should soon use it. Check applyChanges"
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

	^ (self previewPresenterClass newApplication: self application)
		changesFrom: changes scopes: scopes;
		refactoring: self refactoring;
		openModal
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

	self subclassResponsibility
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
ReInteractionDriver >> selectDialog [
	
	^ selectDialog ifNil: [ selectDialog := self defaultSelectDialog ].
	
]

{ #category : 'configuration' }
ReInteractionDriver >> selectDialog: aDialog [
	
	selectDialog := aDialog 
	
]
