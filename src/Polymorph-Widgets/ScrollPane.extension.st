Extension { #name : #ScrollPane }

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> adoptPaneColor: paneColor [
	"Match the pane colour."
	
	super adoptPaneColor: paneColor.
	scrollBar adoptPaneColor: paneColor.
	hScrollBar adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self borderWidth > 0 ifTrue: [
		self borderStyle: self borderStyleToUse]
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> borderStyle: aBorderStyle [
	"Optimised when no change."
	
	self borderStyle = aBorderStyle ifTrue: [^self].
	super borderStyle: aBorderStyle.
	self setScrollDeltas
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> borderStyleToUse [
	"Answer the borderStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme scrollPaneNormalBorderStyleFor: self]
		ifFalse: [self theme scrollPaneDisabledBorderStyleFor: self]
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> getMenuSelector [

	^ getMenuSelector
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> getMenuSelector: aSymbol [
	"Set the menu selector."

	getMenuSelector := aSymbol
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> hScrollValue: scrollValue [
	"Set the horizontal scroll value via the scrollbar itself."

	hScrollBar setValue: scrollValue
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> handlesMouseWheel: evt [
	"Do I want to receive mouseWheel events?" 

	^self vIsScrollable or: [ self hIsScrollable ]
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> hideOrShowScrollBar [
	"Hide or show the scrollbar depending on if the pane is scrolled/scrollable."

	"Don't show it if we were told not to."
	(self valueOfProperty: #noScrollBarPlease ifAbsent: [false]) ifTrue: [^self].

	(self vIsScrollbarNeeded not and: [ self isScrolledFromTop not ]) ifTrue: [self vHideScrollBar].
	(self vIsScrollbarNeeded or: [ self isScrolledFromTop ]) ifTrue: [self vShowScrollBar].

]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> minHeight [
	"Answer the minimum height."

	| noVPlease noHPlease minH |
	noVPlease := self valueOfProperty: #noVScrollBarPlease ifAbsent: [ false ].
	noHPlease := self valueOfProperty: #noHScrollBarPlease ifAbsent: [ false ].
	minH := noVPlease
		ifTrue: [ 
			noHPlease
				ifTrue: [ 1 ]
				ifFalse: [ self scrollBarThickness ] ]
		ifFalse: [ 
			noHPlease
				ifTrue: [ self scrollBarThickness * 3 ]
				ifFalse: [ self scrollBarThickness * 4 + 2 ] ].
	^ minH max: super minHeight
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> minWidth [
	"Answer the minimum width."

	|noVPlease noHPlease minW|
	noVPlease := self valueOfProperty: #noVScrollBarPlease ifAbsent: [false]. 
	noHPlease := self valueOfProperty: #noHScrollBarPlease ifAbsent: [false]. 
	minW := noVPlease
		ifTrue: [noHPlease
					ifTrue: [1]
					ifFalse: [self scrollBarThickness * 3]]
		ifFalse: [noHPlease
					ifTrue: [self scrollBarThickness + 20]
					ifFalse: [self scrollBarThickness * 3 + 2]].
	^minW max: super minWidth
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> mouseWheel: event [ 
	"Handle a mouseWheel event."
	
	event isUp ifTrue: [ ^ scrollBar scrollUp: 3 ].
	event isDown ifTrue: [ ^ scrollBar scrollDown: 3 ].
	event isLeft  ifTrue: [ ^ hScrollBar scrollLeft: 3 ].
	event isRight  ifTrue: [ ^ hScrollBar scrollRight: 3 ].
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> scrollBarThickness [
	"Includes border"
	^ self theme scrollbarThickness
	
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> scrollValue [
	"Answer the values of the scrollbars as a point."

	^hScrollBar value @ scrollBar value
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> scrollValue: aPoint [
	self 
		hScrollValue: aPoint x;
		vScrollValue: aPoint y.
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> themeChanged [
	"The current theme has changed.
	Update any dependent visual aspects."

	scrollBar theme: self theme.
	hScrollBar theme: self theme.
	super themeChanged
]

{ #category : #'*Polymorph-Widgets' }
ScrollPane >> vScrollValue: scrollValue [
	"Set the vertical scroll value via the scrollbar itself."

	scrollBar setValue: scrollValue
]
