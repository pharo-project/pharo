"
I represent a bridge between refactoring core and UI. I am abstract class and you should use my subclasses to invoke specific refactorings.

I am a driver object responsible for invoking refactorings. I am responsible for:
- gathering information from user that are needed to perform a refactoring
- checking preconditions and raising an error if they are not fulfilled
- checking braking changes and warning user if they exist
- and finally, invoking refactoring


"
Class {
	#name : 'RBInteractionDriver',
	#superclass : 'Object',
	#instVars : [
		'model',
		'scopes',
		'refactoring',
		'previewPresenterClass',
		'selectDialog',
		'requestDialog',
		'informDialog',
		'stoppedBeforeApplyingRefactoring'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'testing' }
RBInteractionDriver class >> isAbstract [

	^ self == RBInteractionDriver
]

{ #category : 'displaying' }
RBInteractionDriver class >> menuItemString [
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self refactoringClass menuItemString
]

{ #category : 'displaying' }
RBInteractionDriver class >> refactoringClass [

	self subclassResponsibility
]

{ #category : 'execution' }
RBInteractionDriver >> application [
	"For now use the singleton. Normally the application should be passed from Calypso and the command"
	
	^ StPharoApplication current

]

{ #category : 'execution' }
RBInteractionDriver >> applyChanges [

	| applied |
	applied := self openPreviewWithChanges: self changes.
	"it looks like there is a bug in Spec"
	"stoppedBeforeApplyingRefactoring := applied isCancelled not
	isCancelled returns always true so I will use it when this will be fixed in spec."
]

{ #category : 'execution' }
RBInteractionDriver >> changes [
	"Drivers should not call generateChanges of Refactoring because generateChanges is performing the preconditions and raising errors 
	The API method generateChanges is for refactoring scripting."
	
	^ self subclassResponsibility 

]

{ #category : 'private' }
RBInteractionDriver >> defaultInformDialog [

  ^ self application newInform
]

{ #category : 'configuration' }
RBInteractionDriver >> defaultRequestDialog [
	"We do not use a subclassResponsibility because all the drivers do not have to."

	^ SpRequestDialog new
]

{ #category : 'configuration' }
RBInteractionDriver >> defaultSelectDialog [
	"We do not use a subclassResponsibility because all the drivers do not have to."
	
	^ self
	
]

{ #category : 'execution' }
RBInteractionDriver >> furtherActionFor: aReport [
	
	aReport browse
]

{ #category : 'private' }
RBInteractionDriver >> informConditions: conditions [ 
	"The receiver has multiple failed applicability conditions and more than one could fail, inform all failures"

	conditions do: [ : cond | self inform: cond errorString ].
]

{ #category : 'private' }
RBInteractionDriver >> informDialog [
	"I'm hook to inject mock in tests. The driver should specialize defaultInformDialog"
	
	^ informDialog ifNil: [ informDialog := self defaultInformDialog ]
]

{ #category : 'private' }
RBInteractionDriver >> informDialog: aDialog [

	 informDialog := aDialog
]

{ #category : 'initialization' }
RBInteractionDriver >> initialize [

	super initialize.
	stoppedBeforeApplyingRefactoring := false.
	"for now unused but we should soon use it. Check applyChanges"
]

{ #category : 'accessing' }
RBInteractionDriver >> model [
	"Answer the receiver's <RBBrowserEnvironment>"

	^ model
]

{ #category : 'accessing' }
RBInteractionDriver >> model: aRBBrowserEnvironment [ 
	model := aRBBrowserEnvironment
]

{ #category : 'execution' }
RBInteractionDriver >> openPreviewWithChanges: changes [
	
	^ (self previewPresenterClass
		 changes: changes
		 scopes: scopes)
		application: self application;
		refactoring: self refactoring;
		openModal
]

{ #category : 'configuration' }
RBInteractionDriver >> previewPresenterClass [

	^ previewPresenterClass ifNil: [ previewPresenterClass := StRefactoringPreviewPresenter ] 
	
	
]

{ #category : 'configuration' }
RBInteractionDriver >> previewPresenterClass: aClass [

	 previewPresenterClass := aClass
	
	
]

{ #category : 'resources' }
RBInteractionDriver >> refactoring [
	^ refactoring
]

{ #category : 'utilities' }
RBInteractionDriver >> refactoringScopeOn: aScope [

	^ RBNamespace onEnvironment: aScope asRBEnvironment
]

{ #category : 'private' }
RBInteractionDriver >> requestDialog [
	"I'm hook for the injection of mock in tests. Subclasses should only redefine defaultRequestDialog if needed."
	
	^ requestDialog ifNil: [ requestDialog := self defaultRequestDialog ]
]

{ #category : 'accessing' }
RBInteractionDriver >> requestDialog: aStRequestDialog [

	requestDialog := aStRequestDialog
]

{ #category : 'execution' }
RBInteractionDriver >> runRefactoring [

	self subclassResponsibility
]

{ #category : 'configuration' }
RBInteractionDriver >> selectDialog [
	
	^ selectDialog ifNil: [ selectDialog := self defaultSelectDialog ].
	
]

{ #category : 'configuration' }
RBInteractionDriver >> selectDialog: aDialog [
	
	selectDialog := aDialog 
	
]
