Extension { #name : #String }

{ #category : #'*Morphic-Base' }
String >> asDraggableMorph [

	^ self asStringMorph
]

{ #category : #'*Morphic-Base' }
String >> asMorph [ 
	"Answer the receiver as a StringMorph"

	^ StringMorph contents: self

"'bugs black blood' asMorph openInHand"
]

{ #category : #'*Morphic-Base' }
String >> asStringMorph [ 
	"Answer the receiver as a StringMorph"

	^ StringMorph contents: self

"'bugs black blood' asStringMorph openInHand"
]

{ #category : #'*Morphic-Base-Pluggable Widgets' }
String >> beginsWith: aString fromList: aMorph [

	^ self trimBoth asLowercase beginsWith: aString
]

{ #category : #'*Morphic-Base-Widgets' }
String >> heightToDisplayInList: aList [

	^ aList font height
]

{ #category : #'*Morphic-Base' }
String >> indentationIfBlank: aBlock [ 
	"Answer the number of leading tabs in the receiver.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value."
	| reader leadingTabs lastSeparator cr tab ch |
	cr := Character cr.
	tab := Character tab.
	reader := self readStream.
	leadingTabs := 0.
	[ reader atEnd not and: [ (ch := reader next) = tab ] ] whileTrue: [ leadingTabs := leadingTabs + 1 ].
	lastSeparator := leadingTabs + 1.
	[ reader atEnd not and: [ ch isSeparator and: [ ch ~= cr ] ] ] whileTrue: 
		[ lastSeparator := lastSeparator + 1.
		ch := reader next ].
	(lastSeparator = self size or: [ ch = cr ]) ifTrue: [ ^ aBlock value: leadingTabs ].
	^ leadingTabs
]

{ #category : #'*Morphic-Base-Widgets' }
String >> listRenderOn: aCanvas atRow: aRow bounds: bounds color: color backgroundColor: backgroundColor from: aMorph [

	aCanvas
		drawString: self
		in: bounds
		font: aMorph font
		color: color
]

{ #category : #'*Morphic-Base-Widgets' }
String >> widthToDisplayInList: aList [

	^ aList font widthOfStringOrText: self contents
]
