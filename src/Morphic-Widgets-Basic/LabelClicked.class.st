"
I am an announcement raised when someone click on the label of a CheckboxMorph
"
Class {
	#name : #LabelClicked,
	#superclass : #Announcement,
	#instVars : [
		'source',
		'stateChanged'
	],
	#category : 'Morphic-Widgets-Basic'
}

{ #category : #'instance creation' }
LabelClicked class >> source: source stateChanged: stateChanged [

	^ self new
		source: source;
		stateChanged: stateChanged;
		yourself
]

{ #category : #accessing }
LabelClicked >> source [
	^ source
]

{ #category : #accessing }
LabelClicked >> source: anObject [
	source := anObject
]

{ #category : #accessing }
LabelClicked >> stateChanged [
	^ stateChanged
]

{ #category : #accessing }
LabelClicked >> stateChanged: anObject [
	stateChanged := anObject
]
