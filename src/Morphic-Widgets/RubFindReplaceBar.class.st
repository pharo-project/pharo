"
I am the inline find and replace bar of a code editor, opened by Cmd+F (see FindReplaceService>>openFindBar).

I am a ruler of the RubScrolledTextMorph of the text area I search in: I float over its top-right corner, I follow it when it moves or is resized, and I live only as long as the search. Esc (in me or in the editor) or the close button removes me and gives the keyboard focus back to the editor.

Find row:    [>] [ find field ] [Aa] [ab] [.*]  3 of 12  [^] [v] [x]
Replace row: [ replace field ] [Replace] [All]   (shown by the [>] toggle)

- Typing in the find field searches incrementally from where the search started and shows the match count and the position of the current match (""3 of 12"", ""No results"", ""999+"" when there are more than FindReplaceService class>>maxMatchCount matches).
- Enter selects the next match and Shift+Enter the previous one, wrapping around. In the replace field, Enter replaces the current match.
- Aa, ab and .* toggle case sensitivity, whole words and regular expressions.

I use the theme colors only, so I follow the current theme (light or dark).
On large texts I debounce the search while the user types (see #searchDelay).
"
Class {
	#name : 'RubFindReplaceBar',
	#superclass : 'RubScrolledTextExtra',
	#instVars : [
		'service',
		'findField',
		'replaceField',
		'expandButton',
		'caseButton',
		'wordButton',
		'regexButton',
		'countLabel',
		'previousButton',
		'nextButton',
		'closeButton',
		'replaceButton',
		'replaceAllButton',
		'replaceVisible',
		'searchOrigin',
		'updatingFields',
		'findBoxBounds',
		'replaceBoxBounds'
	],
	#classVars : [
		'LastFindText',
		'ReplaceVisible'
	],
	#category : 'Morphic-Widgets-Editing-FindReplaceService',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-FindReplaceService'
}

{ #category : 'querying' }
RubFindReplaceBar class >> key [

	^ #findReplaceBar
]

{ #category : 'instance creation' }
RubFindReplaceBar class >> on: aFindReplaceService [

	^ self basicNew
		  setService: aFindReplaceService;
		  initialize;
		  yourself
]

{ #category : 'layout' }
RubFindReplaceBar >> buttonSize [
	^ 22
]

{ #category : 'accessing' }
RubFindReplaceBar >> caseButton [

	^ caseButton
]

{ #category : 'actions' }
RubFindReplaceBar >> close [
	"Remove me from the editor. Use FindReplaceService>>closeFindBar to close me, so that the focus goes back to the editor."

	self removeAlarm: #runSearch.
	self removeAlarm: #updateCount.
	LastFindText := self findText.
	self paragraphProvider ifNotNil: [ :scrolledText | scrolledText withoutRuler: self ]
]

{ #category : 'actions' }
RubFindReplaceBar >> closeFromBar [

	service closeFindBar
]

{ #category : 'accessing' }
RubFindReplaceBar >> countLabel [

	^ countLabel
]

{ #category : 'accessing' }
RubFindReplaceBar >> countString [
	"Answer the text of the match count: '3 of 12', '? of 12' when the selection is not a match, 'No results', or 'Invalid regex'."

	| ranges total current |
	self findText ifEmpty: [ ^ '' ].
	service hasInvalidRegex ifTrue: [ ^ 'Invalid regex' ].
	ranges := service matchRanges.
	ranges ifEmpty: [ ^ 'No results' ].
	total := service matchesTruncated
		         ifTrue: [ ranges size printString , '+' ]
		         ifFalse: [ ranges size printString ].
	current := service currentMatchIndex.
	^ (current > 0
		   ifTrue: [ current printString ]
		   ifFalse: [ '?' ]) , ' of ' , total
]

{ #category : 'drawing' }
RubFindReplaceBar >> drawOn: aCanvas [
	"A floating card with a soft shadow, holding inset input boxes. All the colors come from the theme."

	| card radius |
	card := self cardBounds.
	radius := 6.
	1 to: self shadowMargin do: [ :i |
		aCanvas
			drawPolygon: (RubFindBarButton roundedVertices: ((card translateBy: 0 @ 1) expandBy: i) radius: radius + i)
			color: (self shadowColor alpha: self shadowColor alpha / (i + 2))
			borderWidth: 0
			borderColor: Color transparent ].
	aCanvas
		drawPolygon: (RubFindBarButton roundedVertices: card radius: radius)
		color: self cardColor
		borderWidth: 1
		borderColor: self theme borderColor.
	self drawBox: findBoxBounds focused: findField textArea hasKeyboardFocus on: aCanvas.
	replaceVisible ifTrue: [ self drawBox: replaceBoxBounds focused: replaceField textArea hasKeyboardFocus on: aCanvas ].
	"a thin separator between the count and the navigation buttons"
	aCanvas
		fillRectangle: (previousButton left - 5 @ (previousButton top + 4) extent: 1 @ (previousButton height - 8))
		color: self theme borderColor
]

{ #category : 'accessing' }
RubFindReplaceBar >> expandButton [

	^ expandButton
]

{ #category : 'accessing' }
RubFindReplaceBar >> findField [

	^ findField
]

{ #category : 'events' }
RubFindReplaceBar >> findFieldChanged [

	updatingFields ifTrue: [ ^ self ].
	self scheduleSearch
]

{ #category : 'accessing' }
RubFindReplaceBar >> findText [

	^ findField text asString
]

{ #category : 'actions' }
RubFindReplaceBar >> findNext [

	self runPendingSearch.
	service findNext.
	self updateCount
]

{ #category : 'actions' }
RubFindReplaceBar >> findPrevious [

	self runPendingSearch.
	service findPrevious.
	self updateCount
]

{ #category : 'actions' }
RubFindReplaceBar >> focusFindField [

	findField textArea selectAll.
	findField takeKeyboardFocus
]

{ #category : 'event handling' }
RubFindReplaceBar >> handleKeyDown: anEvent from: aFieldArea [
	"Answer true when the key was handled by me: Enter / Shift+Enter navigate (or replace in the replace field), Esc closes."

	| key |
	key := anEvent key.
	key = KeyboardKey escape ifTrue: [
		self closeFromBar.
		^ true ].
	(key = KeyboardKey enter or: [ key = KeyboardKey keypadEnter ]) ifFalse: [ ^ false ].
	aFieldArea == replaceField textArea
		ifTrue: [
			(anEvent commandKeyPressed or: [ anEvent controlKeyPressed ])
				ifTrue: [ self replaceAll ]
				ifFalse: [ self replace ] ]
		ifFalse: [
			anEvent shiftPressed
				ifTrue: [ self findPrevious ]
				ifFalse: [ self findNext ] ].
	^ true
]

{ #category : 'event handling' }
RubFindReplaceBar >> handlesMouseDown: anEvent [

	^ true
]

{ #category : 'initialization' }
RubFindReplaceBar >> initialize [

	super initialize.
	updatingFields := false.
	replaceVisible := ReplaceVisible ifNil: [ false ].
	self clipSubmorphs: true.
	findField := self newFieldWithGhostText: 'Find'.
	replaceField := self newFieldWithGhostText: 'Replace'.
	findField textArea announcer when: RubTextChanged send: #findFieldChanged to: self.
	expandButton := RubFindBarButton glyph: #chevronRight help: 'Toggle replace' action: [ self toggleReplace ].
	caseButton := (RubFindBarButton label: 'Aa' help: 'Match case' action: [ self toggleCase ]) beToggle.
	wordButton := (RubFindBarButton label: 'ab' help: 'Match whole word' action: [ self toggleWholeWord ]) beToggle.
	regexButton := (RubFindBarButton label: '.*' help: 'Use regular expression' action: [ self toggleRegex ]) beToggle.
	countLabel := StringMorph contents: ''.
	countLabel font: self theme labelFont.
	previousButton := RubFindBarButton glyph: #up help: 'Previous match (Shift+Enter)' action: [ self findPrevious ].
	nextButton := RubFindBarButton glyph: #down help: 'Next match (Enter)' action: [ self findNext ].
	closeButton := RubFindBarButton glyph: #close help: 'Close (Esc)' action: [ self closeFromBar ].
	replaceButton := (RubFindBarButton label: 'Replace' help: 'Replace (Enter)' action: [ self replace ]) bePill.
	replaceAllButton := (RubFindBarButton label: 'All' help: 'Replace all (Cmd+Enter)' action: [ self replaceAll ]) bePill.
	{ findField. expandButton. caseButton. wordButton. regexButton. countLabel. previousButton. nextButton. closeButton. replaceField. replaceButton. replaceAllButton }
		do: [ :each | self addMorphBack: each ].
	self updateToggles.
	self extent: self preferredWidth @ self preferredHeight
]

{ #category : 'testing' }
RubFindReplaceBar >> isOver [

	^ true
]

{ #category : 'testing' }
RubFindReplaceBar >> isReplaceVisible [

	^ replaceVisible
]

{ #category : 'layout' }
RubFindReplaceBar >> layoutContents [
	"Lay out my submorphs by hand: no layout policy runs on each change.
	Find row:    > [ find field          Aa ab .* ]  3 of 18 | ^ v x
	Replace row:   [ replace field                 ] (Replace) (All)"

	| card pad size x y rightX boxRight countWidth buttonY |
	card := self cardBounds.
	pad := self padding.
	size := self buttonSize.
	y := card top + pad.
	buttonY := y + (self rowHeight - size // 2).
	x := card left + pad.
	expandButton bounds: (x @ buttonY extent: 18 @ size).
	x := x + 18 + 4.
	"right side, from the right edge"
	rightX := card right - pad.
	{ closeButton. nextButton. previousButton } do: [ :each |
		each bounds: (rightX - size @ buttonY extent: size @ size).
		rightX := rightX - size - 2 ].
	rightX := rightX - 7. "room for the separator"
	countWidth := (self theme labelFont widthOfString: '999 of 999+') + 4.
	countLabel position: rightX - countWidth @ (y + (self rowHeight - countLabel height // 2)).
	boxRight := rightX - countWidth - 8.
	"the find box holds the field and the toggles"
	rightX := boxRight - 3.
	{ regexButton. wordButton. caseButton } do: [ :each |
		each bounds: (rightX - 20 @ (y + (self rowHeight - 20 // 2)) extent: 20 @ 20).
		rightX := rightX - 20 - 1 ].
	findBoxBounds := x @ y corner: boxRight @ (y + self rowHeight).
	findField bounds: (x + 6 @ (y + (self rowHeight - findField height // 2)) corner: rightX - 2 @ (y + (self rowHeight - findField height // 2) + findField height)).
	replaceField visible: replaceVisible.
	replaceButton visible: replaceVisible.
	replaceAllButton visible: replaceVisible.
	replaceVisible ifFalse: [ ^ replaceBoxBounds := nil ].
	y := y + self rowHeight + pad.
	buttonY := y + (self rowHeight - size // 2).
	replaceAllButton bounds: (card right - pad - replaceAllButton minimumWidth @ buttonY extent: replaceAllButton minimumWidth @ size).
	replaceButton bounds: (replaceAllButton left - 4 - replaceButton minimumWidth @ buttonY extent: replaceButton minimumWidth @ size).
	replaceBoxBounds := x @ y corner: boxRight @ (y + self rowHeight).
	replaceField bounds: (x + 6 @ (y + (self rowHeight - replaceField height // 2)) corner: boxRight - 6 @ (y + (self rowHeight - replaceField height // 2) + replaceField height))
]

{ #category : 'accessing' }
RubFindReplaceBar >> level [
	"Negative: I am drawn in front of the text."

	^ -1
]

{ #category : 'geometry' }
RubFindReplaceBar >> manageLayoutInBounds: aRectangle [
	"Float over the top-right corner of the text, left of the vertical scrollbar. The text keeps all its bounds."

	| width |
	width := self preferredWidth min: aRectangle width - (self theme scrollbarThickness + 8).
	self bounds: (aRectangle topRight + ((width + self theme scrollbarThickness + 2) negated @ 3) extent: width @ self preferredHeight).
	self layoutContents.
	^ aRectangle
]

{ #category : 'events' }
RubFindReplaceBar >> matchSelected: aRange [

	self keepMatchVisible.
	self updateCount
]

{ #category : 'events' }
RubFindReplaceBar >> matchesChanged [
	"The text or the search options changed: recount at once in a small text, after a short pause in a large one (the user may still be typing in the editor)"

	| delay |
	delay := self searchDelay.
	(delay = 0 or: [ self world isNil ]) ifTrue: [ ^ self updateCount ].
	self removeAlarm: #updateCount.
	self addAlarm: #updateCount after: delay
]

{ #category : 'event handling' }
RubFindReplaceBar >> mouseDown: anEvent [
	"A click in the bar background focuses the find field instead of reaching the text below"

	findField takeKeyboardFocus
]

{ #category : 'initialization' }
RubFindReplaceBar >> newFieldWithGhostText: aString [
	"A borderless field: I draw its box (see #drawBox:focused:on:), so that the toggles can sit inside it"

	| field |
	field := RubFindReplaceField new.
	field
		withGhostText: aString;
		borderWidth: 0;
		color: Color transparent.
	field textArea backgroundColor: Color transparent.
	"The box draws the focus ring, rounded"
	field scrollPane drawFocusBorder: false.
	field textArea announcer
		when: MorphGotFocus send: #changed to: self;
		when: MorphLostFocus send: #changed to: self.
	^ field
]

{ #category : 'events' }
RubFindReplaceBar >> openedWithFindText: aString [
	"Called by the service each time Cmd+F opens or refocuses me. The search starts at the selection."

	| text |
	searchOrigin := service selectionInterval ifNil: [ 1 ] ifNotNil: [ :interval | interval first ].
	text := aString ifEmpty: [ LastFindText ifNil: [ '' ] ].
	text = self findText ifFalse: [
		updatingFields := true.
		[ findField setText: text ] ensure: [ updatingFields := false ].
		service findText: text ].
	replaceField text asString = service replaceText ifFalse: [
		updatingFields := true.
		[ replaceField setText: service replaceText ] ensure: [ updatingFields := false ] ].
	self updateToggles.
	self updateCount.
	self focusFindField
]

{ #category : 'layout' }
RubFindReplaceBar >> preferredHeight [
	| rows |
	rows := replaceVisible
		        ifTrue: [ self rowHeight * 2 + self padding ]
		        ifFalse: [ self rowHeight ].
	^ rows + (self padding * 2) + (self shadowMargin * 2)
]

{ #category : 'layout' }
RubFindReplaceBar >> preferredWidth [
	^ 460
]

{ #category : 'accessing' }
RubFindReplaceBar >> regexButton [

	^ regexButton
]

{ #category : 'actions' }
RubFindReplaceBar >> replace [

	self runPendingSearch.
	service replaceText: replaceField text asString.
	service replace.
	self updateCount
]

{ #category : 'actions' }
RubFindReplaceBar >> replaceAll [

	self runPendingSearch.
	service replaceText: replaceField text asString.
	service replaceAll.
	self updateCount
]

{ #category : 'accessing' }
RubFindReplaceBar >> replaceField [

	^ replaceField
]

{ #category : 'layout' }
RubFindReplaceBar >> rowHeight [
	"The height of an input box: the field plus a little air"

	^ findField textFieldHeight + 6 max: 26
]

{ #category : 'search' }
RubFindReplaceBar >> runPendingSearch [
	"Run the search scheduled while typing now, before navigating or replacing."

	self findText = service findString ifFalse: [ self runSearch ]
]

{ #category : 'search' }
RubFindReplaceBar >> runSearch [
	"Search the text of the find field incrementally: select the first match from where the search started."

	self removeAlarm: #runSearch.
	service findText: self findText.
	self findText ifNotEmpty: [ service selectNearestMatchFrom: searchOrigin ].
	self updateCount
]

{ #category : 'search' }
RubFindReplaceBar >> scheduleSearch [
	"Search at once in a small text. In a large text, wait until the user stops typing."

	| delay |
	delay := self searchDelay.
	(delay = 0 or: [ self world isNil ]) ifTrue: [ ^ self runSearch ].
	self removeAlarm: #runSearch.
	self addAlarm: #runSearch after: delay
]

{ #category : 'search' }
RubFindReplaceBar >> searchDelay [
	"Milliseconds to wait after a keystroke before searching: none in usual methods, a little in large texts, more for a regex search that goes through the regex engine."

	| size |
	size := service textArea ifNil: [ 0 ] ifNotNil: [ :area | area text size ].
	size < 50000 ifTrue: [ ^ 0 ].
	^ service isRegex
		  ifTrue: [ 300 ]
		  ifFalse: [ 150 ]
]

{ #category : 'events' }
RubFindReplaceBar >> selectionChanged [

	super selectionChanged.
	self updateCount
]

{ #category : 'accessing' }
RubFindReplaceBar >> service [

	^ service
]

{ #category : 'initialization' }
RubFindReplaceBar >> setService: aFindReplaceService [

	service := aFindReplaceService
]

{ #category : 'event handling' }
RubFindReplaceBar >> themeChanged [
	countLabel font: self theme labelFont.
	self updateCount.
	super themeChanged
]

{ #category : 'actions' }
RubFindReplaceBar >> toggleCase [

	service caseSensitive: service caseSensitive not.
	self updateToggles
]

{ #category : 'actions' }
RubFindReplaceBar >> toggleRegex [

	service isRegex: service isRegex not.
	self updateToggles
]

{ #category : 'actions' }
RubFindReplaceBar >> toggleReplace [

	replaceVisible := replaceVisible not.
	ReplaceVisible := replaceVisible.
	self updateToggles.
	self paragraphProvider ifNotNil: [ :scrolledText | scrolledText manageLayout ].
	replaceVisible
		ifTrue: [ replaceField takeKeyboardFocus ]
		ifFalse: [ findField takeKeyboardFocus ]
]

{ #category : 'actions' }
RubFindReplaceBar >> toggleWholeWord [

	service entireWordsOnly: service entireWordsOnly not.
	self updateToggles
]

{ #category : 'updating' }
RubFindReplaceBar >> updateCount [

	| string failed hasMatches |
	string := self countString.
	failed := string = 'No results' or: [ string = 'Invalid regex' ].
	hasMatches := string notEmpty and: [ failed not ].
	countLabel contents: string.
	service textArea ifNotNil: [ :area | area changed ].
	countLabel color: (failed
			 ifTrue: [ self theme dangerTextColor ]
			 ifFalse: [ self theme textColor mixed: 0.7 with: self theme backgroundColor ]).
	{ previousButton. nextButton. replaceButton. replaceAllButton } do: [ :each | each enabled: hasMatches ]
]

{ #category : 'updating' }
RubFindReplaceBar >> updateToggles [

	caseButton selected: service caseSensitive.
	wordButton selected: service entireWordsOnly.
	regexButton selected: service isRegex.
	expandButton glyph: (replaceVisible
			 ifTrue: [ #chevronDown ]
			 ifFalse: [ #chevronRight ]).
	self extent: self width @ self preferredHeight.
	self layoutContents.
	self changed
]

{ #category : 'accessing' }
RubFindReplaceBar >> wordButton [

	^ wordButton
]

{ #category : 'layout' }
RubFindReplaceBar >> shadowMargin [
	"Room around the card for its soft shadow"

	^ 3
]

{ #category : 'layout' }
RubFindReplaceBar >> padding [
	^ 6
]

{ #category : 'layout' }
RubFindReplaceBar >> cardBounds [
	"The visible card: my bounds without the shadow margin"

	^ self bounds insetBy: self shadowMargin
]

{ #category : 'drawing' }
RubFindReplaceBar >> accentColor [
	"The theme selection color, pushed toward the text color so that it reads as an accent in both light and dark themes"

	^ self theme selectionColor mixed: 0.6 with: self theme textColor
]

{ #category : 'drawing' }
RubFindReplaceBar >> cardColor [
	"The card is lifted a little from the editor background, toward the text color: lighter in a dark theme, darker in a light one"

	^ self theme backgroundColor mixed: 0.95 with: self theme textColor
]

{ #category : 'drawing' }
RubFindReplaceBar >> boxColor [
	^ self theme backgroundColor
]

{ #category : 'drawing' }
RubFindReplaceBar >> shadowColor [
	^ self theme menuShadowColor
]

{ #category : 'drawing' }
RubFindReplaceBar >> drawBox: aRectangle focused: aBoolean on: aCanvas [
	"An input box, inset in the card. The focused one gets an accent ring."

	aRectangle ifNil: [ ^ self ].
	aBoolean ifTrue: [
		"a soft outer glow makes the focus ring readable in light themes too"
		aCanvas
			drawPolygon: (RubFindBarButton roundedVertices: (aRectangle expandBy: 1) radius: 5)
			color: Color transparent
			borderWidth: 1
			borderColor: (self accentColor alpha: 0.45) ].
	aCanvas
		drawPolygon: (RubFindBarButton roundedVertices: aRectangle radius: 4)
		color: self boxColor
		borderWidth: 1
		borderColor: (aBoolean
				 ifTrue: [ self accentColor ]
				 ifFalse: [ self theme borderColor ])
]

{ #category : 'events' }
RubFindReplaceBar >> keepMatchVisible [
	"The selected match was scrolled into view, but it may sit under me: scroll the text down until it shows below me."

	| area pane rects matchRect card |
	area := service textArea ifNil: [ ^ self ].
	pane := area scrollPane ifNil: [ ^ self ].
	rects := area selectionRects.
	rects ifEmpty: [ ^ self ].
	matchRect := (pane scroller transformFrom: pane) localBoundsToGlobal: (rects first merge: rects last).
	card := self cardBounds.
	(matchRect intersects: card) ifFalse: [ ^ self ].
	pane scrollBy: 0 @ (card bottom - matchRect top + 4)
]
