Class {
	#name : #DummyFindReplaceService,
	#superclass : #FindReplaceService,
	#instVars : [
		'dialog',
		'textAreaHolder'
	],
	#category : #'Rubric-Editing-FindReplaceService'
}

{ #category : #accessing }
DummyFindReplaceService >> caseSensitive [
	^ caseSensitive ifNil: [caseSensitive := RubAbstractTextArea caseSensitiveFinds]
]

{ #category : #services }
DummyFindReplaceService >> dialogIsActiveFor: aTextArea [

 ^ false 
]

{ #category : #services }
DummyFindReplaceService >> find [

	^ nil
]

{ #category : #services }
DummyFindReplaceService >> findNext [

	^ nil
]

{ #category : #updating }
DummyFindReplaceService >> findPolicyChanged [
]

{ #category : #accessing }
DummyFindReplaceService >> findText: aStringOrText [
	
	^ false
]

{ #category : #initialization }
DummyFindReplaceService >> initialize [
	super initialize.
	
	textAreaHolder := WeakArray new: 1 
]

{ #category : #services }
DummyFindReplaceService >> replace [

]

{ #category : #services }
DummyFindReplaceService >> replaceAll [

]

{ #category : #accessing }
DummyFindReplaceService >> searchBackwards: aBoolean [
	searchBackwards := aBoolean.
	self updateFindStartIndex.
	self findPolicyChanged

]

{ #category : #updating }
DummyFindReplaceService >> updateFindStartIndex [

]

{ #category : #'user-interface' }
DummyFindReplaceService >> whenDialogDeleted [

]

{ #category : #'events handling' }
DummyFindReplaceService >> whenFindReplaceWindowRequired: anAnnouncement [

]

{ #category : #'events handling' }
DummyFindReplaceService >> whenTextAreaGotFocus: anAnnounce [

]

{ #category : #'events handling' }
DummyFindReplaceService >> whenTextAreaLostFocus: anAnnounce [

]

{ #category : #'events handling' }
DummyFindReplaceService >> whenTextAreaSelectionChanged: anAnnouncement [ 

]
