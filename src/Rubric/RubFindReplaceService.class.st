"
RubFindReplaceService default newDialog openInWorld
RubFindReplaceService 
"
Class {
	#name : #RubFindReplaceService,
	#superclass : #Model,
	#instVars : [
		'findText',
		'replaceText',
		'caseSensitive',
		'entireWordsOnly',
		'wrapAround',
		'searchBackwards',
		'findStartIndex',
		'isRegex',
		'dialog',
		'textAreaHolder'
	],
	#classVars : [
		'Singleton'
	],
	#category : 'Rubric-Editing-FindReplaceService'
}

{ #category : #cleanup }
RubFindReplaceService class >> cleanUp [

	Singleton := nil
]

{ #category : #accessing }
RubFindReplaceService class >> default [
	^ Singleton ifNil: [ Singleton := self new ]

]

{ #category : #accessing }
RubFindReplaceService class >> dialog [
	^ self default dialog
]

{ #category : #accessing }
RubFindReplaceService >> caseSensitive [
	^ caseSensitive ifNil: [caseSensitive := TextEditor caseSensitiveFinds]
]

{ #category : #accessing }
RubFindReplaceService >> caseSensitive: aBoolean [
	caseSensitive := aBoolean.
	self findPolicyChanged
]

{ #category : #accessing }
RubFindReplaceService >> convertedFindString [
	| specials |
	specials := '^$:\+*[]()?'.
	^String 
		streamContents: [:s | self findString 
			do: [:c | (specials includes: c) ifTrue: [s nextPut:$\].
				s nextPut: c]]
]

{ #category : #user-interface }
RubFindReplaceService >> dialog [
	^ dialog ifNil: [ dialog := self newDialog ]
	
]

{ #category : #user-interface }
RubFindReplaceService >> dialogIsActiveFor: aTextArea [
	^ self dialog world notNil and: [ self textArea = aTextArea  ]
]

{ #category : #accessing }
RubFindReplaceService >> entireWordsOnly [
	^ entireWordsOnly ifNil: [entireWordsOnly := false]
]

{ #category : #accessing }
RubFindReplaceService >> entireWordsOnly: aBoolean [
	entireWordsOnly := aBoolean.
	self findPolicyChanged
]

{ #category : #services }
RubFindReplaceService >> find [
	| where |
	self setStartIndex.
	where := self findAndSelectRegex.
	where ifNil: [ self textArea flash ].
	^ where
]

{ #category : #private }
RubFindReplaceService >> findAndSelect [
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

{ #category : #private }
RubFindReplaceService >> findAndSelectRegex [
	^ self findRegex ifNotNil: [ self findAndSelect ]
]

{ #category : #services }
RubFindReplaceService >> findNext [
	| where  |
	findStartIndex 
		ifNil: [findStartIndex := self textArea editor startIndex].
	findStartIndex > 0 
		ifTrue: [where := self textArea findNextString: self findString startingAt: findStartIndex].
	(where isNil and: [self wrapAround])
		ifTrue: [where := self textArea findNextString: self findString asString startingAt: 1].
	^ where

]

{ #category : #updating }
RubFindReplaceService >> findPolicyChanged [
	self changed: #findPolicy.
	self textArea whenFindTextChanged: RubFindTextChanged new
]

{ #category : #services }
RubFindReplaceService >> findRegex [
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
RubFindReplaceService >> findStartIndex [
	^ findStartIndex
]

{ #category : #accessing }
RubFindReplaceService >> findStartIndex: anInteger [
	findStartIndex := anInteger
]

{ #category : #accessing }
RubFindReplaceService >> findString [
	^ self findText asString
]

{ #category : #accessing }
RubFindReplaceService >> findText [
	^ (findText ifNil: [findText := '' asText. findText]) asString
]

{ #category : #accessing }
RubFindReplaceService >> findText: aStringOrText [
	findText := aStringOrText asText.
	self findPolicyChanged.
	self textArea whenFindTextChanged: RubFindTextChanged new.
	^ true
]

{ #category : #accessing }
RubFindReplaceService >> findText: aStringOrText isRegex: aBoolean [
		self isRegex: aBoolean.
		self findText: aStringOrText.
		self replaceText: ''.

]

{ #category : #accessing }
RubFindReplaceService >> findText: aStringOrText isRegex: isReg caseSensitive: isCaseSensitive entireWordsOnly: forEntireWordsOnly [
		self isRegex: isReg.
		self findText: aStringOrText.
		self entireWordsOnly: forEntireWordsOnly.
		self caseSensitive: isCaseSensitive
]

{ #category : #accessing }
RubFindReplaceService >> findText: aStringOrText isRegex: aBoolean entireWordsOnly: forEntireWordsOnly [
		self isRegex: aBoolean.
		self findText: aStringOrText.
		self entireWordsOnly: forEntireWordsOnly
]

{ #category : #accessing }
RubFindReplaceService >> findTextSilently: aStringOrText [
	findText := aStringOrText asText.
	self findPolicyChanged
]

{ #category : #initialization }
RubFindReplaceService >> initialize [
	super initialize.
	
	textAreaHolder := WeakArray new: 1 
]

{ #category : #accessing }
RubFindReplaceService >> isRegex [
	^ isRegex ifNil: [isRegex := false]
]

{ #category : #accessing }
RubFindReplaceService >> isRegex: aBoolean [
	isRegex := aBoolean.
	self findPolicyChanged

]

{ #category : #user-interface }
RubFindReplaceService >> newDialog [
	| d |
	d := RubFindReplaceDialogWindow new model: self.
	d findPolicyChanged.
	d announcer when: MorphDeleted send: #whenDialogDeleted to: self.
	^ d
	
]

{ #category : #accessing }
RubFindReplaceService >> regexString [
	^ self isRegex 
		ifTrue: [self findString]
		ifFalse: [self convertedFindString]

]

{ #category : #services }
RubFindReplaceService >> replace [
	findStartIndex := self searchBackwards 
		ifTrue: [self textArea editor stopIndex] 
		ifFalse: [self textArea editor startIndex].
	(self find)
		ifNotNil: [self textArea replaceSelectionWith: self replaceText]
]

{ #category : #services }
RubFindReplaceService >> replaceAll [
	| startIdx |
	startIdx := self wrapAround ifTrue: [1] ifFalse: [self textArea editor selectionInterval first].
	self textArea replaceAll: self findRegex with: self replaceText startingAt: startIdx

]

{ #category : #accessing }
RubFindReplaceService >> replaceText [
	^ replaceText ifNil: [replaceText := '']
]

{ #category : #accessing }
RubFindReplaceService >> replaceText: aStringOrText [
	replaceText := aStringOrText asString.
	self findPolicyChanged

]

{ #category : #accessing }
RubFindReplaceService >> searchBackwards [
	^ searchBackwards ifNil: [searchBackwards := false]
]

{ #category : #accessing }
RubFindReplaceService >> searchBackwards: aBoolean [
	searchBackwards := aBoolean.
	self updateFindStartIndex.
	self findPolicyChanged

]

{ #category : #accessing }
RubFindReplaceService >> selectionRegexString [
	^ self convertedFindString

]

{ #category : #private }
RubFindReplaceService >> setStartIndex [
	findStartIndex
		ifNil: [ findStartIndex := self searchBackwards
				ifTrue: [ self textArea editor stopIndex ]
				ifFalse: [ self textArea editor startIndex ] ]
]

{ #category : #accessing }
RubFindReplaceService >> textArea [
	^ textAreaHolder at: 1 
]

{ #category : #accessing }
RubFindReplaceService >> textArea: aTextArea [
	textAreaHolder at: 1 put: aTextArea.
	aTextArea announcer 
		when: RubFindReplaceWindowRequired send: #whenFindReplaceWindowRequired: to: self. 

]

{ #category : #updating }
RubFindReplaceService >> updateFindStartIndex [
	| si |
	self textArea 
		ifNil: [ ^self ]
		ifNotNil: [ :aTextArea |
			aTextArea changed.
			self changed: #replaceEnabled.
			si := aTextArea selectionInterval.
			self findStartIndex: (self searchBackwards ifTrue: [si first - 1] ifFalse: [si last + 1]) ]
]

{ #category : #user-interface }
RubFindReplaceService >> whenDialogDeleted [
	dialog := nil.

]

{ #category : #'events handling' }
RubFindReplaceService >> whenFindReplaceWindowRequired: anAnnouncement [
	self findText
		ifEmpty: [ 
			self textArea hasSelection
				ifTrue: [ self findText: self textArea selection ] ].
	self dialog open
]

{ #category : #'events handling' }
RubFindReplaceService >> whenTextAreaGotFocus: anAnnounce [
	self textArea: anAnnounce morph.
	self findText: self findText.
	self changed: #findText.
	self changed: #caseSensitive.
	self changed: #replaceEnabled.

]

{ #category : #'events handling' }
RubFindReplaceService >> whenTextAreaLostFocus: anAnnounce [
	self dialog isInWorld ifFalse: [ ^self ].
	self textArea whenFindTextChanged: RubFindTextChanged new

]

{ #category : #'events handling' }
RubFindReplaceService >> whenTextAreaSelectionChanged: anAnnouncement [ 
	self updateFindStartIndex
]

{ #category : #accessing }
RubFindReplaceService >> wrapAround [
	^ wrapAround ifNil: [wrapAround := true]
]

{ #category : #accessing }
RubFindReplaceService >> wrapAround: aBoolean [
	wrapAround := aBoolean.
	self findPolicyChanged


]
