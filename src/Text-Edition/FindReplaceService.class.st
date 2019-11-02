"
Model for a find and replace service in editors
"
Class {
	#name : #FindReplaceService,
	#superclass : #Model,
	#instVars : [
		'findText',
		'replaceText',
		'caseSensitive',
		'entireWordsOnly',
		'wrapAround',
		'searchBackwards',
		'findStartIndex',
		'isRegex'
	],
	#category : #'Text-Edition'
}

{ #category : #'instance creation' }
FindReplaceService class >> newFor: textArea [

	"sublasses may use the text area object"
	^ self new
]

{ #category : #accessing }
FindReplaceService >> caseSensitive [
	^ caseSensitive ifNil: [caseSensitive := TextEditor caseSensitiveFinds]
]

{ #category : #accessing }
FindReplaceService >> caseSensitive: aBoolean [
	caseSensitive := aBoolean.
	self findPolicyChanged
]

{ #category : #accessing }
FindReplaceService >> convertedFindString [
	| specials |
	specials := '^$:\+*[]()?'.
	^String 
		streamContents: [:s | self findString 
			do: [:c | (specials includes: c) ifTrue: [s nextPut:$\].
				s nextPut: c]]
]

{ #category : #accessing }
FindReplaceService >> entireWordsOnly [
	^ entireWordsOnly ifNil: [entireWordsOnly := false]
]

{ #category : #accessing }
FindReplaceService >> entireWordsOnly: aBoolean [
	entireWordsOnly := aBoolean.
	self findPolicyChanged
]

{ #category : #services }
FindReplaceService >> findInTextMorph: aTextMorph [
	| where  |
	findStartIndex 
		ifNil: [findStartIndex := self searchBackwards 
			ifTrue: [aTextMorph editor stopIndex]
			ifFalse: [aTextMorph editor startIndex]].
	findStartIndex > 0 
		ifTrue: [where := aTextMorph
			findAndSelect: self findRegex
			startingAt: findStartIndex
			searchBackwards: self searchBackwards].
	(where isNil and: [self wrapAround])
		ifTrue: [ | idx |
			idx := self searchBackwards ifTrue: [aTextMorph editor string size] ifFalse: [1].
			where := aTextMorph
				findAndSelect: self findRegex
				startingAt: idx
				searchBackwards: self searchBackwards].
	where ifNil: [aTextMorph flash].
	^ where

]

{ #category : #services }
FindReplaceService >> findNextSubstring: aSubstring inTextMorph: aTextMorph [
	| where  |
	findStartIndex 
		ifNil: [findStartIndex := aTextMorph editor startIndex].
	findStartIndex > 0 
		ifTrue: [where := aTextMorph findNextString: aSubstring asString startingAt: findStartIndex].
	(where isNil and: [self wrapAround])
		ifTrue: [where := aTextMorph findNextString: aSubstring asString startingAt: 1].
	^ where

]

{ #category : #accessing }
FindReplaceService >> findPolicyChanged [

	self changed: #findPolicy
]

{ #category : #services }
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
		do: [self changed: #regexError. 
			^ '' asRegex]

]

{ #category : #accessing }
FindReplaceService >> findStartIndex [
	^ findStartIndex
]

{ #category : #accessing }
FindReplaceService >> findStartIndex: anInteger [
	findStartIndex := anInteger
]

{ #category : #accessing }
FindReplaceService >> findString [
	^ self findText asString
]

{ #category : #accessing }
FindReplaceService >> findText [
	^ (findText ifNil: [findText := '' asText. findText]) asString
]

{ #category : #accessing }
FindReplaceService >> findText: aStringOrText [
	findText := aStringOrText asText.
	self findPolicyChanged.
]

{ #category : #accessing }
FindReplaceService >> findText: aStringOrText isRegex: aBoolean [
		self isRegex: aBoolean.
		self findText: aStringOrText.
		self replaceText: ''.

]

{ #category : #accessing }
FindReplaceService >> findText: aStringOrText isRegex: isReg caseSensitive: isCaseSensitive entireWordsOnly: forEntireWordsOnly [
		self isRegex: isReg.
		self findText: aStringOrText.
		self entireWordsOnly: forEntireWordsOnly.
		self caseSensitive: isCaseSensitive
]

{ #category : #accessing }
FindReplaceService >> findText: aStringOrText isRegex: aBoolean entireWordsOnly: forEntireWordsOnly [
		self isRegex: aBoolean.
		self findText: aStringOrText.
		self entireWordsOnly: forEntireWordsOnly
]

{ #category : #accessing }
FindReplaceService >> isRegex [
	^ isRegex ifNil: [isRegex := false]
]

{ #category : #accessing }
FindReplaceService >> isRegex: aBoolean [
	isRegex := aBoolean.
	self findPolicyChanged

]

{ #category : #accessing }
FindReplaceService >> regexString [
	^ self isRegex 
		ifTrue: [self findString]
		ifFalse: [self convertedFindString]

]

{ #category : #services }
FindReplaceService >> replaceAllInTextMorph: aTextMorph [
	| startIdx |
	startIdx := self wrapAround ifTrue: [1] ifFalse: [aTextMorph editor selectionInterval first].
	aTextMorph replaceAll: self findRegex with: self replaceText startingAt: startIdx

]

{ #category : #services }
FindReplaceService >> replaceInTextMorph: aTextMorph [

	findStartIndex := self searchBackwards 
		ifTrue: [aTextMorph editor stopIndex] 
		ifFalse: [aTextMorph editor startIndex].
	(self findInTextMorph: aTextMorph)
		ifNotNil: [aTextMorph replaceSelectionWith: self replaceText]
]

{ #category : #accessing }
FindReplaceService >> replaceText [
	^ replaceText ifNil: [replaceText := '']
]

{ #category : #accessing }
FindReplaceService >> replaceText: aStringOrText [
	replaceText := aStringOrText asString.
	self findPolicyChanged

]

{ #category : #accessing }
FindReplaceService >> searchBackwards [
	^ searchBackwards ifNil: [searchBackwards := false]
]

{ #category : #accessing }
FindReplaceService >> searchBackwards: aBoolean [
	searchBackwards := aBoolean.
	self findPolicyChanged

]

{ #category : #accessing }
FindReplaceService >> selectionRegexString [
	^ self convertedFindString

]

{ #category : #updating }
FindReplaceService >> updateFindStartIndexForTextMorph: aTextMorph [
	| si |
	si := aTextMorph editor selectionInterval.
	self findStartIndex: (self searchBackwards ifTrue: [si first - 1] ifFalse: [si last + 1])
]

{ #category : #accessing }
FindReplaceService >> wrapAround [
	^ wrapAround ifNil: [wrapAround := true]
]

{ #category : #accessing }
FindReplaceService >> wrapAround: aBoolean [
	wrapAround := aBoolean.
	self findPolicyChanged


]
