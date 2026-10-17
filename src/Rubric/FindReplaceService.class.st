"
I am the base class of the find and replace services used by Rubric text areas.

I hold the search state (find text, replacement text, case sensitivity, entire word only, wrap around, search direction, start index) and implement the find and replace engine over the text area I am attached to with #textArea:.

My subclasses provide the user interface: SpRubFindReplaceService opens a Spec dialog, while RubNullFindReplaceService does nothing and is used by the text areas that have no find and replace support (see RubAbstractTextArea class>>defaultFindReplaceServiceClass).
"
Class {
	#name : 'FindReplaceService',
	#superclass : 'Object',
	#instVars : [
		'findText',
		'replaceText',
		'caseSensitive',
		'entireWordsOnly',
		'wrapAround',
		'searchBackwards',
		'findStartIndex',
		'isRegex',
		'textAreaHolder'
	],
	#category : 'Rubric-Editing-FindReplaceService',
	#package : 'Rubric',
	#tag : 'Editing-FindReplaceService'
}

{ #category : 'instance creation' }
FindReplaceService class >> newFor: textArea [

	"sublasses may use the text area object"
	^ self new
]

{ #category : 'accessing' }
FindReplaceService >> caseSensitive [
	^ caseSensitive ifNil: [caseSensitive := false ]
]

{ #category : 'accessing' }
FindReplaceService >> caseSensitive: aBoolean [
	caseSensitive := aBoolean.
	self findPolicyChanged
]

{ #category : 'accessing' }
FindReplaceService >> convertedFindString [
	| specials |
	specials := '|^$:\+*[](){}?.'.
	^String
		streamContents: [:s | self findString
			do: [:c | (specials includes: c) ifTrue: [s nextPut:$\].
				s nextPut: c]]
]

{ #category : 'accessing' }
FindReplaceService >> entireWordsOnly [
	^ entireWordsOnly ifNil: [entireWordsOnly := false]
]

{ #category : 'accessing' }
FindReplaceService >> entireWordsOnly: aBoolean [
	entireWordsOnly := aBoolean.
	self findPolicyChanged
]

{ #category : 'services' }
FindReplaceService >> find [

	| where |
	self setStartIndex.
	where := self findAndSelectRegex.
	where ifNil: [ self textArea flash ].
	^ where
]

{ #category : 'services' }
FindReplaceService >> findAndSelect [

	| where |
	findStartIndex > 0 ifTrue: [ where := self textArea findAndSelect: self findRegex startingAt: findStartIndex searchBackwards: self searchBackwards ].
	(where isNil and: [ self wrapAround ]) ifTrue: [
			| idx |
			idx := self searchBackwards
				       ifTrue: [ self textArea editor string size ]
				       ifFalse: [ 1 ].
			where := self textArea findAndSelect: self findRegex startingAt: idx searchBackwards: self searchBackwards ].
	^ where
]

{ #category : 'services' }
FindReplaceService >> findAndSelectRegex [

	^ self findRegex ifNotNil: [ self findAndSelect ]
]

{ #category : 'services' }
FindReplaceService >> findNext [

	| where |
	findStartIndex ifNil: [ findStartIndex := self textArea editor startIndex ].
	findStartIndex > 0 ifTrue: [ where := self textArea findNextString: self findString startingAt: findStartIndex ].
	(where isNil and: [ self wrapAround ]) ifTrue: [ where := self textArea findNextString: self findString asString startingAt: 1 ].
	where ifNil: [ self textArea flash ].
	^ where
]

{ #category : 'accessing' }
FindReplaceService >> findPolicyChanged [

	self textArea ifNotNil: [ :textArea | textArea whenFindTextChanged: RubFindTextChanged new ]
]

{ #category : 'services' }
FindReplaceService >> findRegex [
	| s |
	self regexString ifEmpty: [ ^nil ].
	s := self entireWordsOnly
		ifTrue: ['\<', self regexString, '\>']
		ifFalse: [ self regexString].
	[^ self caseSensitive
			ifTrue: [s asRegex]
			ifFalse: [s asRegexIgnoringCase]]
		on: Error
		do: [^ '' asRegex]
]

{ #category : 'accessing' }
FindReplaceService >> findStartIndex [
	^ findStartIndex
]

{ #category : 'accessing' }
FindReplaceService >> findStartIndex: anInteger [
	findStartIndex := anInteger
]

{ #category : 'accessing' }
FindReplaceService >> findString [
	^ self findText asString
]

{ #category : 'accessing' }
FindReplaceService >> findText [
	^ (findText ifNil: [findText := '' asText. findText]) asString
]

{ #category : 'accessing' }
FindReplaceService >> findText: aStringOrText [

	findText := aStringOrText asText.
	self findPolicyChanged
]

{ #category : 'accessing' }
FindReplaceService >> findText: aStringOrText isRegex: aBoolean [
		self isRegex: aBoolean.
		self findText: aStringOrText.
		self replaceText: ''
]

{ #category : 'accessing' }
FindReplaceService >> findText: aStringOrText isRegex: isReg caseSensitive: isCaseSensitive entireWordsOnly: forEntireWordsOnly [
		self isRegex: isReg.
		self findText: aStringOrText.
		self entireWordsOnly: forEntireWordsOnly.
		self caseSensitive: isCaseSensitive
]

{ #category : 'accessing' }
FindReplaceService >> findText: aStringOrText isRegex: aBoolean entireWordsOnly: forEntireWordsOnly [
		self isRegex: aBoolean.
		self findText: aStringOrText.
		self entireWordsOnly: forEntireWordsOnly
]

{ #category : 'initialization' }
FindReplaceService >> initialize [

	super initialize.

	textAreaHolder := WeakArray new: 1
]

{ #category : 'accessing' }
FindReplaceService >> isRegex [
	^ isRegex ifNil: [isRegex := false]
]

{ #category : 'accessing' }
FindReplaceService >> isRegex: aBoolean [
	isRegex := aBoolean.
	self findPolicyChanged
]

{ #category : 'accessing' }
FindReplaceService >> regexString [
	^ self isRegex
		ifTrue: [self findString]
		ifFalse: [self convertedFindString]
]

{ #category : 'services' }
FindReplaceService >> replace [

	findStartIndex := self searchBackwards
		                  ifTrue: [ self textArea editor stopIndex ]
		                  ifFalse: [ self textArea editor startIndex ].
	self find ifNotNil: [ self textArea replaceSelectionWith: self replaceText ]
]

{ #category : 'services' }
FindReplaceService >> replaceAll [

	| startIdx |
	startIdx := self wrapAround
		            ifTrue: [ 1 ]
		            ifFalse: [ self textArea editor selectionInterval first ].
	self textArea replaceAll: self findRegex with: self replaceText startingAt: startIdx
]

{ #category : 'accessing' }
FindReplaceService >> replaceText [
	^ replaceText ifNil: [replaceText := '']
]

{ #category : 'accessing' }
FindReplaceService >> replaceText: aStringOrText [
	replaceText := aStringOrText asString.
	self findPolicyChanged
]

{ #category : 'accessing' }
FindReplaceService >> searchBackwards [
	^ searchBackwards ifNil: [searchBackwards := false]
]

{ #category : 'accessing' }
FindReplaceService >> searchBackwards: aBoolean [

	searchBackwards := aBoolean.
	self updateFindStartIndex.
	self findPolicyChanged
]

{ #category : 'accessing' }
FindReplaceService >> selectionRegexString [
	^ self convertedFindString
]

{ #category : 'updating' }
FindReplaceService >> setStartIndex [

	findStartIndex ifNil: [
			findStartIndex := self searchBackwards
				                  ifTrue: [ self textArea editor stopIndex ]
				                  ifFalse: [ self textArea editor startIndex ] ]
]

{ #category : 'accessing' }
FindReplaceService >> textArea [

	^ textAreaHolder at: 1
]

{ #category : 'accessing' }
FindReplaceService >> textArea: aTextArea [

	| oldTextArea |
	oldTextArea := textAreaHolder at: 1 ifAbsent: nil.
	oldTextArea == aTextArea ifTrue: [ ^ self ].
	oldTextArea ifNotNil: [ oldTextArea announcer unsubscribe: self ].
	textAreaHolder at: 1 put: aTextArea.
	aTextArea announcer when: RubFindReplaceWindowRequired send: #whenFindReplaceWindowRequired: to: self
]

{ #category : 'updating' }
FindReplaceService >> updateFindStartIndex [

	| si |
	self textArea
		ifNil: [ ^ self ]
		ifNotNil: [ :aTextArea |
				aTextArea changed.
				si := aTextArea selectionInterval.
				self findStartIndex: (self searchBackwards
						 ifTrue: [ si first - 1 ]
						 ifFalse: [ si last + 1 ]) ]
]

{ #category : 'event handling' }
FindReplaceService >> whenTextAreaSelectionChanged: anAnnouncement [

	self updateFindStartIndex
]

{ #category : 'accessing' }
FindReplaceService >> wrapAround [
	^ wrapAround ifNil: [wrapAround := true]
]

{ #category : 'accessing' }
FindReplaceService >> wrapAround: aBoolean [
	wrapAround := aBoolean.
	self findPolicyChanged
]
