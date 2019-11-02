"
I am a FindReplace service that allows to use Spec based dialog as a replacement of a native Morphic find & replace dialog. 

I can currently work only with the default Morphic Spec backend.
"
Class {
	#name : #SpRubFindReplaceService,
	#superclass : #FindReplaceService,
	#instVars : [
		'dialog',
		'dialogWindow',
		'textAreaHolder',
		'app'
	],
	#category : #'Rubric-SpecFindReplaceDialog'
}

{ #category : #accessing }
SpRubFindReplaceService >> caseSensitive [
	^ caseSensitive ifNil: [caseSensitive := RubAbstractTextArea caseSensitiveFinds]
]

{ #category : #services }
SpRubFindReplaceService >> dialogIsActiveFor: aTextArea [

 ^ false 
]

{ #category : #'events handling' }
SpRubFindReplaceService >> discardDialog [

	dialog := nil.
	dialogWindow := nil.
]

{ #category : #services }
SpRubFindReplaceService >> find [
	| where |
	self setStartIndex.
	where := self findAndSelectRegex.
	where ifNil: [ self textArea flash ].
	^ where
]

{ #category : #services }
SpRubFindReplaceService >> findAndSelect [
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
SpRubFindReplaceService >> findAndSelectRegex [
	^ self findRegex ifNotNil: [ self findAndSelect ]
]

{ #category : #services }
SpRubFindReplaceService >> findNext [

	^ nil
]

{ #category : #updating }
SpRubFindReplaceService >> findPolicyChanged [
]

{ #category : #services }
SpRubFindReplaceService >> findRegex [
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
SpRubFindReplaceService >> findText: aStringOrText [
	
	super findText: aStringOrText.
	self textArea whenFindTextChanged: RubFindTextChanged new.
	^ true
]

{ #category : #initialization }
SpRubFindReplaceService >> initialize [
	super initialize.
	
	textAreaHolder := WeakArray new: 1 
]

{ #category : #'events handling' }
SpRubFindReplaceService >> openDialog [

	dialogWindow ifNotNil: [ 
		dialogWindow close ].

	app := SpApplication new.
	dialog := SpRubFindReplaceDialog newApplication: app model: self.
	dialog updateFromModel.
				
	dialogWindow := dialog openDialogWithSpec: #defaultSpec.
]

{ #category : #services }
SpRubFindReplaceService >> replace [
	findStartIndex := self searchBackwards 
		ifTrue: [self textArea editor stopIndex] 
		ifFalse: [self textArea editor startIndex].
	(self find)
		ifNotNil: [self textArea replaceSelectionWith: self replaceText]
]

{ #category : #services }
SpRubFindReplaceService >> replaceAll [
	| startIdx |
	startIdx := self wrapAround ifTrue: [1] ifFalse: [self textArea editor selectionInterval first].
	self textArea replaceAll: self findRegex with: self replaceText startingAt: startIdx

]

{ #category : #accessing }
SpRubFindReplaceService >> searchBackwards: aBoolean [
	searchBackwards := aBoolean.
	self updateFindStartIndex.
	self findPolicyChanged

]

{ #category : #updating }
SpRubFindReplaceService >> setStartIndex [
	findStartIndex
		ifNil: [ findStartIndex := self searchBackwards
				ifTrue: [ self textArea editor stopIndex ]
				ifFalse: [ self textArea editor startIndex ] ]
]

{ #category : #services }
SpRubFindReplaceService >> textArea [
	^ textAreaHolder at: 1 
]

{ #category : #services }
SpRubFindReplaceService >> textArea: aTextArea [
	textAreaHolder at: 1 put: aTextArea.
	aTextArea announcer 
		when: RubFindReplaceWindowRequired send: #whenFindReplaceWindowRequired: to: self. 

]

{ #category : #updating }
SpRubFindReplaceService >> updateFindStartIndex [
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
SpRubFindReplaceService >> whenDialogDeleted [

]

{ #category : #'events handling' }
SpRubFindReplaceService >> whenFindReplaceWindowRequired: anAnnouncement [

	self textArea hasSelection
		ifTrue: [ self findText: self textArea selection ].

	self openDialog.
]

{ #category : #'events handling' }
SpRubFindReplaceService >> whenTextAreaGotFocus: anAnnounce [
	self textArea: anAnnounce morph.
]

{ #category : #'events handling' }
SpRubFindReplaceService >> whenTextAreaLostFocus: anAnnounce [

]

{ #category : #'events handling' }
SpRubFindReplaceService >> whenTextAreaSelectionChanged: anAnnouncement [ 
	self updateFindStartIndex
]
