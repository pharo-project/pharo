"
A SpecDialogWindow is a window made for embedding Spec models into dialog windows

"
Class {
	#name : #SpDialogWindow,
	#superclass : #DialogWindow,
	#instVars : [
		'toolbar'
	],
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #actions }
SpDialogWindow >> addInitialPanel [
	"Do nothing here because Spec will take care of adding the content"
]

{ #category : #protocol }
SpDialogWindow >> cancelAction: aBlock [

	self toolbar cancelAction: aBlock 
]

{ #category : #protocol }
SpDialogWindow >> cancelled [

	^ self toolbar cancelled
]

{ #category : #actions }
SpDialogWindow >> escapePressed [

	self model triggerCancelAction
]

{ #category : #focus }
SpDialogWindow >> giveFocusToNextFrom: aModel [ 

	^ self model giveFocusToNextFrom: aModel 
]

{ #category : #focus }
SpDialogWindow >> giveFocusToPreviousFrom: aModel [ 

	^ self model giveFocusToPreviousFrom: aModel
]

{ #category : #'open/close' }
SpDialogWindow >> initialExtent [ 

	^ self model
		ifNil: [ super initialExtent ]
		ifNotNil: [:m | m initialExtent
						ifNil: [ super initialExtent ]]
]

{ #category : #initialization }
SpDialogWindow >> initialize [
	super initialize.
	self beResizeable
]

{ #category : #accessing }
SpDialogWindow >> newButtonRow [

	^ self toolbar ifNotNil: [ : t | t buildWithSpec ]
]

{ #category : #protocol }
SpDialogWindow >> okAction: aBlock [

	self toolbar okAction: aBlock 
]

{ #category : #protocol }
SpDialogWindow >> setToolbarFrom: aBlock [
	| newToolbar |
	
	newToolbar := aBlock value.
	self toolbar: newToolbar.
	self model presenter focusOrder addLast: toolbar.
	toolbar focusOrder addLast: self model
]

{ #category : #accessing }
SpDialogWindow >> toolbar [
	"Lazy initialization because this method is called efore the initialize method"
	
	^ toolbar
]

{ #category : #accessing }
SpDialogWindow >> toolbar: anObject [
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
