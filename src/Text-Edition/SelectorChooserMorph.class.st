"
A SelectorChooserMorph is an IdentifierChooserMorph specialized to allow a selector query from a selector prefix and for a TextMorph. 
When the prefix starts with an uppercase, then, only globals names and class names that begins with the prefix are shown.
Otherwise, all systems symbols that begins with the prefix are presented (the string comparison is not case sensitive).
It takes the keyboard focus when a navigation key (up, down, left or right key) is first pressed in the TextMorph from which it has been opened.

Instance Variables
	prefix: <String>
	requestorPos <Point>
	
prefix
	- The prefix which has been used to compute the labels list.

requestorPos
	- private, used to be able to automatically close the menu if the position of the requestor is changing
"
Class {
	#name : #SelectorChooserMorph,
	#superclass : #IdentifierChooserMorph,
	#instVars : [
		'prefix',
		'requestorPos'
	],
	#category : #'Text-Edition'
}

{ #category : #opening }
SelectorChooserMorph class >> popUpPrefix: aPrefix from: aTextMorph [ 
	Cursor wait
		showWhile: [| selectors chooser |
			selectors := self selectorsFromPrefix: aPrefix.
			selectors size = 1
				ifTrue: [aTextMorph editor editingState chosenSelector: selectors first for: aTextMorph.
					^ nil].
			selectors isEmpty
				ifFalse: [chooser := self new
								chooseBlock: [:chosen | aTextMorph editor editingState chosenSelector: chosen for: aTextMorph];
								 requestor: aTextMorph;
								 prefix: aPrefix; 
								 labels: selectors;
								 yourself.
					chooser open].
			^ chooser]
]

{ #category : #accessing }
SelectorChooserMorph class >> selectorsFromPrefix: aPrefix [
	"returns the value of labels from a selector prefix"
	| key applicants |
	aPrefix isEmptyOrNil  ifTrue: [^ #()].
	applicants := aPrefix first canBeGlobalVarInitial
		ifTrue: [Array streamContents: [:strm |
			Symbol allSymbolTablesDo: [:each |
				(each notEmpty and: [each first canBeGlobalVarInitial])
					ifTrue: [strm nextPut: each]]]]
		ifFalse: [Array streamContents: [:strm |
			Symbol allSymbolTablesDo: [:each |
				(each notEmpty and: [each first canBeGlobalVarInitial not])
					ifTrue: [strm nextPut: each]]]].
	key := aPrefix asLowercase.
	^ (applicants select: [:each | (each asLowercase beginsWith: key) and: [each noneSatisfy: [:c | c tokenish not]]]) sort

]

{ #category : #action }
SelectorChooserMorph >> choose: index [
	"Set the given choice and ok."
	requestor ifNil: [^ self delete].
	requestor editor editingState selectorChooser ~= self 
		ifTrue: [^ self delete].
	requestor takeKeyboardFocus.
	super choose: index
	
]

{ #category : #action }
SelectorChooserMorph >> close [
	requestor 
		ifNotNil: [requestor takeKeyboardFocus.
			self choose: 0]
]

{ #category : #'submorphs-add/remove' }
SelectorChooserMorph >> comeToFront [
	super comeToFront.
	self setPosition 
]

{ #category : #accessing }
SelectorChooserMorph >> defaultBaseColor [
	^  (self theme menuColorFor: self) muchLighter
]

{ #category : #drawing }
SelectorChooserMorph >> drawCommonPrefixAreasOn: aCanvas [
	 
	requestor ifNil: [^ self].
	prefix 
		ifNotNil: [ self choiceMenus 
			do: [:cm | self drawCommonPrefixAreasOn: aCanvas menu: cm]]

]

{ #category : #drawing }
SelectorChooserMorph >> drawCommonPrefixAreasOn: aCanvas menu: aMenu [
	| firstMenuItem topLeft bottomLeft lastMenuItem w c |
	firstMenuItem := aMenu menuItems first.
	lastMenuItem := aMenu menuItems last.
	w := (firstMenuItem fontToUse widthOfString: prefix).
	topLeft := firstMenuItem boundsInWorld topLeft.
	bottomLeft := lastMenuItem boundsInWorld bottomLeft + ( w @ 0 ).
	c := aMenu color muchDarker.
	aCanvas fillRectangle: (topLeft  corner: bottomLeft) color: (c alpha: 0.3)
]

{ #category : #drawing }
SelectorChooserMorph >> fullDrawOn: aCanvas [
	super fullDrawOn: aCanvas.
	self drawCommonPrefixAreasOn: aCanvas
	
]

{ #category : #'ui-building' }
SelectorChooserMorph >> newContentMorph [ 
	| sp |
	sp := super newContentMorph.
	sp
		updateScrollbars.
	^ sp

]

{ #category : #'ui-building' }
SelectorChooserMorph >> nextPrefixWith: aPrefix [
	| curr pref found lab |
	curr := aPrefix size.
	lab := self labels first.
	found := false.
	[lab size <= curr 
		ifTrue: [found := true]
		ifFalse: [pref := lab copyFrom: 1 to: curr.
			self labels 
				do: [:l | found 
					ifFalse: [(l asString beginsWith: pref)
						ifFalse: [pref := pref allButLast.
							found := true]]]].
	found] whileFalse: [curr := curr + 1].
	^ (pref notNil and: [pref size > aPrefix size])
		ifTrue: [pref]

]

{ #category : #'ui-building' }
SelectorChooserMorph >> open [
	super open. 
	self borderStyle: ( self theme progressBarBorderStyleFor: self).
	self setPosition
]

{ #category : #'ui-building' }
SelectorChooserMorph >> positionInRequestor [
	| caret bottomLeft topLeft editor pos allowedArea |
	requestor ifNil: [^self].
	editor := requestor editor.
	caret := editor stopIndex.
	[editor selectAt: caret.
	bottomLeft := editor paragraph selectionRects first bottomLeft.
	topLeft := requestor owner submorphBounds topLeft.
	pos := topLeft + bottomLeft]
		ensure: [editor selectAt: caret].
	pos := pos + (40 @ 0).
	allowedArea := self allowedArea.
	(pos x + self width) > allowedArea width
		ifTrue: [pos := pos - (self width @ 0) - ((50 + 40)@ 0)].
	(pos y + self height) > allowedArea height
		ifTrue: [pos := (pos x @ (allowedArea height - self height - 5))].
	^ pos.

]

{ #category : #accessing }
SelectorChooserMorph >> prefix: aPrefix [
	"Set the value of labels"
	prefix := aPrefix.

]

{ #category : #'ui-building' }
SelectorChooserMorph >> setPosition [
	self position: self positionInRequestor.

]

{ #category : #stepping }
SelectorChooserMorph >> step [
	[requestorPos 
		ifNil: [requestorPos := requestor owner position]
		ifNotNil: [requestorPos ~= requestor owner position
			ifTrue: [^ self close]].
	(requestor isNil or: [requestor isInWorld not])
		ifTrue: [^ self close].
	requestor editor editingState selectorChooser ~= self 
		ifTrue: [self close]
	] on: Error do: [self delete]

]

{ #category : #stepping }
SelectorChooserMorph >> stepTime [
	^ 500
]

{ #category : #stepping }
SelectorChooserMorph >> wantsSteps [ 
	^ requestor notNil
]
