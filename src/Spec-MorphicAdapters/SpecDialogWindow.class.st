"
A SpecDialogWindow is a window made for embedding Spec models into dialog windows

"
Class {
	#name : #SpecDialogWindow,
	#superclass : #DialogWindow,
	#instVars : [
		'toolbar'
	],
	#category : #'Spec-MorphicAdapters-Support'
}

{ #category : #actions }
SpecDialogWindow >> addInitialPanel [
	"Do nothing here because Spec will take care of adding the content"
]

{ #category : #protocol }
SpecDialogWindow >> cancelAction: aBlock [

	self toolbar cancelAction: aBlock 
]

{ #category : #protocol }
SpecDialogWindow >> cancelled [

	^ self toolbar cancelled
]

{ #category : #actions }
SpecDialogWindow >> escapePressed [

	self model triggerCancelAction
]

{ #category : #focus }
SpecDialogWindow >> giveFocusToNextFrom: aModel [ 

	^ self model giveFocusToNextFrom: aModel 
]

{ #category : #focus }
SpecDialogWindow >> giveFocusToPreviousFrom: aModel [ 

	^ self model giveFocusToPreviousFrom: aModel
]

{ #category : #'open/close' }
SpecDialogWindow >> initialExtent [ 

	^ self model
		ifNil: [ super initialExtent ]
		ifNotNil: [:m | m initialExtent
						ifNil: [ super initialExtent ]]
]

{ #category : #initialization }
SpecDialogWindow >> initialize [
	super initialize.
	self beResizeable
]

{ #category : #accessing }
SpecDialogWindow >> newButtonRow [

	^ self toolbar ifNotNil: [ : t | t buildWithSpec ]
]

{ #category : #protocol }
SpecDialogWindow >> okAction: aBlock [

	self toolbar okAction: aBlock 
]

{ #category : #protocol }
SpecDialogWindow >> setToolbarFrom: aBlock [
	| newToolbar |
	
	newToolbar := aBlock value.
	self toolbar: newToolbar.
	self model presenter focusOrder addLast: toolbar.
	toolbar focusOrder addLast: self model
]

{ #category : #accessing }
SpecDialogWindow >> toolbar [
	"Lazy initialization because this method is called efore the initialize method"
	
	^ toolbar
]

{ #category : #accessing }
SpecDialogWindow >> toolbar: anObject [
	| content |
	
	toolbar := anObject.
	self removeMorph: (content := self submorphs last).
	self 
		addMorph: (self newDialogPanel 
			addMorphBack: content; 
			addMorphBack: self newButtonRow; 
			yourself) 
		frame: (0 @ 0 corner: 1 @ 1)
]
