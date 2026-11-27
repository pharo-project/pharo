"
I am the base class of the find and replace services used by Rubric text areas.

I hold the search state (find text, replacement text, case sensitivity, entire word only, regular expression, wrap around, search direction) and implement the find and replace engine over the text area I am attached to with #textArea:.

The engine works on the list of the ranges of all the matches in the text (#matchRanges). It is computed on demand, at most #maxMatchCount ranges, and cached until the text or the search policy change. Navigation is computed from the current selection of the text area and this list:
- #findNext and #findPrevious select the next or previous match and wrap around if #wrapAround is set;
- #currentMatchIndex answers the index of the match that is selected, if any;
- #replace replaces the selected match and selects the next one. When no match is selected it only selects the next match, so that nothing is replaced unseen;
- #replaceAll replaces every match as a single undoable edit.

Nothing is computed until a search is requested: a text area with an empty find text never pays for me.

When asked by the text area (Cmd+F, see RubFindReplaceWindowRequired), I open a RubFindReplaceBar: an inline bar attached to the scrolled text morph of the text area. When the text area cannot hold the bar, I call #openFallbackFindDialog, which my subclasses can redefine.

My subclasses provide the fallback user interface: SpRubFindReplaceService opens a Spec dialog, while RubNullFindReplaceService does nothing and is used by the text areas that have no find and replace support (see RubAbstractTextArea class>>defaultFindReplaceServiceClass).
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
		'textAreaHolder',
		'matchRanges',
		'matchesTruncated',
		'matchSource',
		'matchSourceSize',
		'findBar',
		'textChangedSubscription'
	],
	#category : 'Morphic-Widgets-Editing-FindReplaceService',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-FindReplaceService'
}

{ #category : 'accessing' }
FindReplaceService class >> maxMatchCount [
	"Matches are counted and highlighted up to this number, so that a search in a huge text stays cheap. Navigation goes on past it."

	^ 999
]

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

{ #category : 'find bar' }
FindReplaceService >> closeFindBar [
	"Close the inline find bar if it is open, and give the focus back to the text area."

	| bar |
	bar := findBar ifNil: [ ^ self ].
	findBar := nil.
	self textArea ifNotNil: [ :area |
		textChangedSubscription ifNotNil: [ :subscription | area announcer removeSubscription: subscription ].
		textChangedSubscription := nil.
		bar close.
		area changed.
		area takeKeyboardFocus ].
	self invalidateMatches
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

{ #category : 'matching' }
FindReplaceService >> currentMatchIndex [
	"Answer the index in #matchRanges of the match that is selected in the text area, or 0 if the selection is not a match."

	| selection |
	selection := self selectionInterval ifNil: [ ^ 0 ].
	selection isEmpty ifTrue: [ ^ 0 ].
	^ self indexOfMatch: selection
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
	"Select the next match in the search direction (see #searchBackwards). Answer its range, or nil if there is none."

	^ self searchBackwards
		  ifTrue: [ self findPrevious ]
		  ifFalse: [ self findNext ]
]

{ #category : 'services' }
FindReplaceService >> findAndSelect [
	"Kept for compatibility. See #find."

	^ self find
]

{ #category : 'services' }
FindReplaceService >> findAndSelectRegex [
	"Kept for compatibility. See #find."

	^ self find
]

{ #category : 'find bar' }
FindReplaceService >> findBar [
	"Answer the inline find bar opened on my text area, or nil when it is closed."

	^ findBar
]

{ #category : 'services' }
FindReplaceService >> findNext [
	"Select the match after the selection, wrapping around if allowed. Answer its range, or nil if there is none."

	^ self selectMatch: self nextMatchRange
]

{ #category : 'accessing' }
FindReplaceService >> findPolicyChanged [

	self invalidateMatches.
	findBar ifNotNil: [ :bar | bar matchesChanged ].
	self textArea ifNotNil: [ :textArea | textArea whenFindTextChanged: RubFindTextChanged new ]
]

{ #category : 'services' }
FindReplaceService >> findPrevious [
	"Select the match before the selection, wrapping around if allowed. Answer its range, or nil if there is none."

	^ self selectMatch: self previousMatchRange
]

{ #category : 'services' }
FindReplaceService >> findRegex [
	"Answer the regex matching the find text with the current options, or nil if the find text is empty or is not a valid regular expression."

	| s |
	self regexString ifEmpty: [ ^ nil ].
	s := self entireWordsOnly
		     ifTrue: [ '\<' , self regexString , '\>' ]
		     ifFalse: [ self regexString ].
	^ [
	  self caseSensitive
		  ifTrue: [ s asRegex ]
		  ifFalse: [ s asRegexIgnoringCase ] ]
		  on: Error
		  do: [ nil ]
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

{ #category : 'testing' }
FindReplaceService >> hasActiveSearch [
	"Answer whether the matches must be highlighted: only while the find bar is open, so that the text areas pay nothing otherwise."

	^ findBar isNotNil and: [ self findString isNotEmpty ]
]

{ #category : 'testing' }
FindReplaceService >> hasInvalidRegex [

	^ self isRegex and: [ self findString isNotEmpty and: [ self findRegex isNil ] ]
]

{ #category : 'matching' }
FindReplaceService >> indexOfMatch: anInterval [
	"Answer the index of the match whose range is anInterval, or 0."

	^ self indexOfMatch: anInterval in: self matchRanges
]

{ #category : 'matching' }
FindReplaceService >> indexOfMatch: anInterval in: ranges [
	"Answer the index in ranges of anInterval, or 0. Binary search: the ranges are sorted and disjoint."

	| low high mid range |
	low := 1.
	high := ranges size.
	[ low <= high ] whileTrue: [
		mid := low + high // 2.
		range := ranges at: mid.
		range first = anInterval first ifTrue: [
			^ range last = anInterval last
				  ifTrue: [ mid ]
				  ifFalse: [ 0 ] ].
		range first < anInterval first
			ifTrue: [ low := mid + 1 ]
			ifFalse: [ high := mid - 1 ] ].
	^ 0
]

{ #category : 'initialization' }
FindReplaceService >> initialize [

	super initialize.

	textAreaHolder := WeakArray new: 1.
	matchesTruncated := false
]

{ #category : 'matching' }
FindReplaceService >> invalidateMatches [

	matchRanges := nil.
	matchSource := nil
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

{ #category : 'private' }
FindReplaceService >> isWordCharacter: aCharacter [

	^ aCharacter isAlphaNumeric or: [ aCharacter = $_ ]
]

{ #category : 'private' }
FindReplaceService >> isWholeWordFrom: start to: stop in: aString [

	(start > 1 and: [ self isWordCharacter: (aString at: start - 1) ]) ifTrue: [ ^ false ].
	(stop < aString size and: [ self isWordCharacter: (aString at: stop + 1) ]) ifTrue: [ ^ false ].
	^ true
]

{ #category : 'matching' }
FindReplaceService >> matchRanges [
	"Answer the ranges of the matches in the text of my text area, sorted, at most #maxMatchCount of them. They are cached until the text or the search policy change."

	| string |
	string := self textArea ifNil: [ ^ #(  ) ] ifNotNil: [ :area | area text string ].
	(matchRanges isNotNil and: [ matchSource == string and: [ matchSourceSize = string size ] ])
		ifTrue: [ ^ matchRanges ].
	matchRanges := self matchRangesIn: string.
	matchSource := string.
	matchSourceSize := string size.
	^ matchRanges
]

{ #category : 'matching' }
FindReplaceService >> matchRangesFrom: start to: stop [
	"Answer the ranges of the matches that intersect start to stop, when the matches are highlighted (see #hasActiveSearch). Used to draw the visible matches."

	| ranges |
	self hasActiveSearch ifFalse: [ ^ #(  ) ].
	"Drawing never searches: it shows the cached matches, which the find bar recomputes (debounced on large texts)"
	ranges := self validCachedMatchRanges ifNil: [ ^ #(  ) ].
	^ ranges select: [ :r | r last >= start and: [ r first <= stop ] ]
]

{ #category : 'matching' }
FindReplaceService >> validCachedMatchRanges [
	"Answer the cached match ranges if they are still valid for the text, or nil. Never searches."

	| string |
	matchRanges ifNil: [ ^ nil ].
	string := self textArea ifNil: [ ^ nil ] ifNotNil: [ :area | area text string ].
	^ (matchSource == string and: [ matchSourceSize = string size ])
		  ifTrue: [ matchRanges ]
		  ifFalse: [ nil ]
]

{ #category : 'matching' }
FindReplaceService >> matchRangesIn: aString [
	"Answer the ranges of the matches in aString, at most #maxMatchCount of them. #matchesTruncated tells whether there are more."

	| ranges |
	ranges := self matchRangesIn: aString from: 1 limit: self class maxMatchCount.
	matchesTruncated := ranges size = self class maxMatchCount and: [
		                    (self matchRangesIn: aString from: ranges last last + 1 limit: 1) notEmpty ].
	^ ranges
]

{ #category : 'matching' }
FindReplaceService >> matchRangesIn: aString from: startIndex limit: maxCount [
	"Answer the ranges of the first maxCount matches in aString that start at or after startIndex. Matches do not overlap and are never empty.
	The plain text search uses the fast String primitives: only a regular expression search goes through the regex engine. In both cases the whole string is searched, so that the word boundaries before startIndex are respected."

	| ranges |
	ranges := OrderedCollection new.
	(self findString isEmpty or: [ maxCount < 1 or: [ startIndex > aString size ] ]) ifTrue: [ ^ #(  ) ].
	self isRegex
		ifTrue: [ self regexRangesIn: aString from: startIndex limit: maxCount into: ranges ]
		ifFalse: [ self plainRangesIn: aString from: startIndex limit: maxCount into: ranges ].
	^ ranges asArray
]

{ #category : 'accessing' }
FindReplaceService >> matchesTruncated [
	"Answer whether the text holds more matches than #maxMatchCount."

	self matchRanges.
	^ matchesTruncated
]

{ #category : 'matching' }
FindReplaceService >> nextMatchRange [
	"Answer the range of the match after the selection, wrapping around if allowed, or nil."

	| ranges selection current index |
	ranges := self matchRanges.
	ranges ifEmpty: [ ^ nil ].
	selection := self selectionInterval ifNil: [ 1 to: 0 ].
	current := selection isEmpty
		           ifTrue: [ 0 ]
		           ifFalse: [ self indexOfMatch: selection ].
	index := current > 0
		         ifTrue: [ current + 1 ]
		         ifFalse: [ ranges findFirst: [ :r | r first >= selection first ] ].
	(index between: 1 and: ranges size) ifTrue: [ ^ ranges at: index ].
	"Past the last counted match: there may be more matches when the count is truncated."
	matchesTruncated ifTrue: [
		| string from found |
		string := self textArea text string.
		from := selection first max: ranges last last + 1.
		found := self matchRangesIn: string from: from limit: 1.
		"Step over the selection when it is itself one of the uncounted matches"
		(found notEmpty and: [ found first = selection ]) ifTrue: [ found := self matchRangesIn: string from: selection last + 1 limit: 1 ].
		found ifNotEmpty: [ ^ found first ] ].
	^ self wrapAround
		  ifTrue: [ ranges first ]
		  ifFalse: [ nil ]
]

{ #category : 'find bar' }
FindReplaceService >> openFallbackFindDialog [
	"The text area cannot hold an inline find bar. My subclasses may open a dialog instead."

	self textArea ifNotNil: [ :area | area flash ]
]

{ #category : 'find bar' }
FindReplaceService >> openFindBar [
	"Open the inline find bar on the scrolled text morph that holds my text area, or focus it if it is already open. Answer the bar, or nil if the text area cannot hold one."

	| area scrolledText |
	area := self textArea ifNil: [ ^ nil ].
	scrolledText := area ownerThatIsA: RubScrolledTextMorph.
	(scrolledText isNil or: [ scrolledText isKindOf: RubTextFieldMorph ]) ifTrue: [ ^ nil ].
	findBar ifNil: [
		findBar := RubFindReplaceBar on: self.
		area withFindReplaceSelection.
		textChangedSubscription := area announcer when: RubTextChanged send: #whenTextAreaTextChanged: to: self.
		scrolledText withRuler: findBar ].
	self invalidateMatches.
	findBar openedWithFindText: self findString.
	^ findBar
]

{ #category : 'private' }
FindReplaceService >> plainRangesIn: aString from: startIndex limit: maxCount into: ranges [

	| pattern size index |
	pattern := self findString.
	size := pattern size.
	index := startIndex.
	[ ranges size < maxCount and: [ (index := aString findString: pattern startingAt: index caseSensitive: self caseSensitive) > 0 ] ]
		whileTrue: [
			(self entireWordsOnly not or: [ self isWholeWordFrom: index to: index + size - 1 in: aString ])
				ifTrue: [
					ranges add: (index to: index + size - 1).
					index := index + size ]
				ifFalse: [ index := index + 1 ] ]
]

{ #category : 'matching' }
FindReplaceService >> previousMatchRange [
	"Answer the range of the match before the selection, wrapping around if allowed, or nil."

	| ranges selection current index text |
	ranges := self matchRanges.
	ranges ifEmpty: [ ^ nil ].
	selection := self selectionInterval ifNil: [ 1 to: 0 ].
	current := selection isEmpty
		           ifTrue: [ 0 ]
		           ifFalse: [ self indexOfMatch: selection ].
	current > 0 ifTrue: [
		current > 1 ifTrue: [ ^ ranges at: current - 1 ] ].
	"The selection may be past the last counted match when the count is truncated."
	(current = 0 and: [ matchesTruncated and: [ selection first > ranges last last ] ]) ifTrue: [
		| found |
		text := self textArea text string.
		found := ranges last.
		[
		(self matchRangesIn: text from: found last + 1 limit: 1)
			ifEmpty: [ false ]
			ifNotEmpty: [ :next | next first first < selection first and: [ found := next first. true ] ] ] whileTrue.
		^ found ].
	current = 0 ifTrue: [
		index := ranges findLast: [ :r | r first < selection first ].
		index > 0 ifTrue: [ ^ ranges at: index ] ].
	^ self wrapAround
		  ifTrue: [ ranges last ]
		  ifFalse: [ nil ]
]

{ #category : 'private' }
FindReplaceService >> regexRangesIn: aString from: startIndex limit: maxCount into: ranges [

	| regex stream |
	regex := self findRegex ifNil: [ ^ self ].
	stream := aString readStream.
	stream position: startIndex - 1.
	regex matchesOnStream: stream do: [ :match |
		match isEmpty ifFalse: [
			ranges add: (stream position - match size + 1 to: stream position).
			ranges size >= maxCount ifTrue: [ ^ self ] ] ]
]

{ #category : 'accessing' }
FindReplaceService >> regexString [
	^ self isRegex
		ifTrue: [self findString]
		ifFalse: [self convertedFindString]
]

{ #category : 'services' }
FindReplaceService >> replace [
	"Replace the selected match with the replacement text and select the next match. When the selection is not a match, only select the next match: nothing is replaced without being shown first. Answer the range of the match that is selected afterwards, or nil."

	| index range caret |
	index := self currentMatchIndex.
	index = 0 ifTrue: [ ^ self findNext ].
	range := self matchRanges at: index.
	self textArea replaceSelectionWith: self replaceText.
	self invalidateMatches.
	caret := range first + self replaceText size.
	self textArea selectFrom: caret to: caret - 1.
	^ self searchBackwards
		ifTrue: [
			self textArea selectFrom: range first to: range first - 1.
			self findPrevious ]
		ifFalse: [ self findNext ]
]

{ #category : 'services' }
FindReplaceService >> replaceAll [
	"Replace all the matches (only those after the selection start when #wrapAround is false) as a single undoable edit. Answer how many matches were replaced."

	| string ranges selection |
	self textArea ifNil: [ ^ 0 ].
	string := self textArea text string.
	selection := self selectionInterval ifNil: [ 1 to: 0 ].
	ranges := self
		          matchRangesIn: string
		          from: (self wrapAround ifTrue: [ 1 ] ifFalse: [ selection first max: 1 ])
		          limit: SmallInteger maxVal.
	ranges ifEmpty: [
		self textArea flash.
		^ 0 ].
	self textArea replaceRanges: ranges with: self replaceText.
	self invalidateMatches.
	findBar ifNotNil: [ :bar | bar matchesChanged ].
	^ ranges size
]

{ #category : 'accessing' }
FindReplaceService >> replaceText [
	^ replaceText ifNil: [replaceText := '']
]

{ #category : 'accessing' }
FindReplaceService >> replaceText: aStringOrText [
	replaceText := aStringOrText asString
]

{ #category : 'accessing' }
FindReplaceService >> searchBackwards [
	^ searchBackwards ifNil: [searchBackwards := false]
]

{ #category : 'accessing' }
FindReplaceService >> searchBackwards: aBoolean [

	searchBackwards := aBoolean
]

{ #category : 'services' }
FindReplaceService >> selectMatch: aRangeOrNil [
	"Select aRangeOrNil in the text area and scroll it into view. Flash when there is no match."

	| area |
	area := self textArea ifNil: [ ^ nil ].
	aRangeOrNil ifNil: [
		area flash.
		findBar ifNotNil: [ :bar | bar matchesChanged ].
		^ nil ].
	area selectFrom: aRangeOrNil first to: aRangeOrNil last.
	area scrollSelectionIntoView: nil.
	findBar ifNotNil: [ :bar | bar matchSelected: aRangeOrNil ].
	area changed.
	^ aRangeOrNil
]

{ #category : 'services' }
FindReplaceService >> selectNearestMatchFrom: anIndex [
	"Select the first match at or after anIndex, wrapping around. Used for the incremental search while the find text is typed: it answers nil and selects nothing when there is no match."

	| range |
	range := self matchRanges
		         detect: [ :r | r first >= anIndex ]
		         ifNone: [ self matchRanges ifEmpty: [ nil ] ifNotEmpty: [ :ranges | ranges first ] ].
	range ifNil: [
		findBar ifNotNil: [ :bar | bar matchesChanged ].
		^ nil ].
	^ self selectMatch: range
]

{ #category : 'accessing' }
FindReplaceService >> selectionInterval [

	^ self textArea ifNotNil: [ :area | area selectionInterval ]
]

{ #category : 'accessing' }
FindReplaceService >> selectionRegexString [
	^ self convertedFindString
]

{ #category : 'updating' }
FindReplaceService >> setStartIndex [
	"Kept for compatibility: the navigation is now computed from the selection."
]

{ #category : 'private' }
FindReplaceService >> subscribeTo: aTextArea [

	aTextArea announcer when: RubFindReplaceWindowRequired send: #whenFindReplaceWindowRequired: to: self
]

{ #category : 'accessing' }
FindReplaceService >> textArea [

	^ self textAreaHolder at: 1
]

{ #category : 'accessing' }
FindReplaceService >> textArea: aTextArea [

	| oldTextArea |
	oldTextArea := self textAreaHolder at: 1 ifAbsent: nil.
	oldTextArea == aTextArea ifTrue: [ ^ self ].
	oldTextArea ifNotNil: [
		self closeFindBar.
		oldTextArea announcer unsubscribe: self ].
	textChangedSubscription := nil.
	self textAreaHolder at: 1 put: aTextArea.
	self invalidateMatches.
	self subscribeTo: aTextArea
]

{ #category : 'private' }
FindReplaceService >> textAreaHolder [
	"Lazy, for the instances created before this variable existed"

	^ textAreaHolder ifNil: [ textAreaHolder := WeakArray new: 1 ]
]

{ #category : 'updating' }
FindReplaceService >> updateFindStartIndex [
	"Kept for compatibility: the navigation is now computed from the selection."
]

{ #category : 'event handling' }
FindReplaceService >> whenFindReplaceWindowRequired: anAnnouncement [
	"Cmd+F: seed the find text with the selection when it holds a single line, then open the inline find bar, or a fallback dialog."

	self textArea ifNotNil: [ :area |
		| selection |
		selection := area selection asString.
		(selection isNotEmpty and: [ (selection includesAny: String crlf) not ]) ifTrue: [ self findText: selection ] ].
	self openFindBar ifNil: [ self openFallbackFindDialog ]
]

{ #category : 'event handling' }
FindReplaceService >> whenTextAreaGotFocus: anAnnouncement [

	self textArea: anAnnouncement morph
]

{ #category : 'event handling' }
FindReplaceService >> whenTextAreaLostFocus: anAnnouncement [
]

{ #category : 'event handling' }
FindReplaceService >> whenTextAreaSelectionChanged: anAnnouncement [

	findBar ifNotNil: [ :bar | bar selectionChanged ]
]

{ #category : 'event handling' }
FindReplaceService >> whenTextAreaTextChanged: anAnnouncement [

	self invalidateMatches.
	findBar ifNotNil: [ :bar | bar matchesChanged ]
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
