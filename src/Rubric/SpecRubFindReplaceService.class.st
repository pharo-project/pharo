Class {
	#name : #SpecRubFindReplaceService,
	#superclass : #FindReplaceService,
	#instVars : [
		'dialog',
		'dialogWindow',
		'textAreaHolder',
		'app'
	],
	#category : #'Rubric-Editing-FindReplaceService'
}

{ #category : #accessing }
SpecRubFindReplaceService class >> newFor: anObject [

	^ super new
]

{ #category : #accessing }
SpecRubFindReplaceService >> caseSensitive [
	^ caseSensitive ifNil: [caseSensitive := RubAbstractTextArea caseSensitiveFinds]
]

{ #category : #services }
SpecRubFindReplaceService >> dialogIsActiveFor: aTextArea [

 ^ false 
]

{ #category : #services }
SpecRubFindReplaceService >> find [
	| where |
	self setStartIndex.
	where := self findAndSelectRegex.
	where ifNil: [ self textArea flash ].
	^ where
]

{ #category : #services }
SpecRubFindReplaceService >> findAndSelect [
	| where |
	findStartIndex > 0
		ifTrue: [ where := self textArea
				findAndSelect: self findRegex
				startingAt: findStartIndex
				searchBackwards: self searchBackwards ].
	(where isNil and: [ self wrapAround ])
		ifTrue: [ | idx |
			idx := self searchBackwards
				ifTrue: [ self textArea editor string size ]
				ifFalse: [ 1 ].
			where := self textArea
				findAndSelect: self findRegex
				startingAt: idx
				searchBackwards: self searchBackwards ].
	^ where
]

{ #category : #services }
SpecRubFindReplaceService >> findAndSelectRegex [
	^ self findRegex ifNotNil: [ self findAndSelect ]
]

{ #category : #services }
SpecRubFindReplaceService >> findNext [

	^ nil
]

{ #category : #updating }
SpecRubFindReplaceService >> findPolicyChanged [
]

{ #category : #services }
SpecRubFindReplaceService >> findRegex [
	| s |
	self regexString ifEmpty: [ ^nil ].
	s := self entireWordsOnly
		ifTrue: ['\<', self regexString, '\>']
		ifFalse: [ self regexString].
	[^ self caseSensitive
			ifTrue: [s asRegex]
			ifFalse: [s asRegexIgnoringCase]]
		on: Error 
		do: [self changed: #regexError. 
			^ '' asRegex]

]

{ #category : #accessing }
SpecRubFindReplaceService >> findText: aStringOrText [
	
	super findText: aStringOrText.
	self textArea whenFindTextChanged: RubFindTextChanged new.
	^ true
]

{ #category : #initialization }
SpecRubFindReplaceService >> initialize [
	super initialize.
	
	textAreaHolder := WeakArray new: 1 
]

{ #category : #'events handling' }
SpecRubFindReplaceService >> openDialog [

	dialog 
		ifNil: [ 
			app := SpApplication new.
			dialog := SpecRubDialog newApplication: app model: self ]
		ifNotNil: [ 
			dialog updateFromModel ].
		
	dialogWindow ifNotNil: [ 
		dialogWindow close ].
	
	dialogWindow := dialog openDialogWithSpec: #defaultSpec.
]

{ #category : #services }
SpecRubFindReplaceService >> replace [

]

{ #category : #services }
SpecRubFindReplaceService >> replaceAll [

]

{ #category : #accessing }
SpecRubFindReplaceService >> searchBackwards: aBoolean [
	searchBackwards := aBoolean.
	self updateFindStartIndex.
	self findPolicyChanged

]

{ #category : #updating }
SpecRubFindReplaceService >> setStartIndex [
	findStartIndex
		ifNil: [ findStartIndex := self searchBackwards
				ifTrue: [ self textArea editor stopIndex ]
				ifFalse: [ self textArea editor startIndex ] ]
]

{ #category : #services }
SpecRubFindReplaceService >> textArea [
	^ textAreaHolder at: 1 
]

{ #category : #services }
SpecRubFindReplaceService >> textArea: aTextArea [
	textAreaHolder at: 1 put: aTextArea.
	aTextArea announcer 
		when: RubFindReplaceWindowRequired send: #whenFindReplaceWindowRequired: to: self. 

]

{ #category : #updating }
SpecRubFindReplaceService >> updateFindStartIndex [
	| si |
	self textArea 
		ifNil: [ ^self ]
		ifNotNil: [ :aTextArea |
			aTextArea changed.
			self changed: #replaceEnabled.
			si := aTextArea selectionInterval.
			self findStartIndex: (self searchBackwards ifTrue: [si first - 1] ifFalse: [si last + 1]) ]
]

{ #category : #'user-interface' }
SpecRubFindReplaceService >> whenDialogDeleted [

]

{ #category : #'events handling' }
SpecRubFindReplaceService >> whenFindReplaceWindowRequired: anAnnouncement [

	self textArea hasSelection
		ifTrue: [ self findText: self textArea selection ].

	self openDialog.
]

{ #category : #'events handling' }
SpecRubFindReplaceService >> whenTextAreaGotFocus: anAnnounce [
	self textArea: anAnnounce morph.
]

{ #category : #'events handling' }
SpecRubFindReplaceService >> whenTextAreaLostFocus: anAnnounce [

]

{ #category : #'events handling' }
SpecRubFindReplaceService >> whenTextAreaSelectionChanged: anAnnouncement [ 
	self updateFindStartIndex
]
