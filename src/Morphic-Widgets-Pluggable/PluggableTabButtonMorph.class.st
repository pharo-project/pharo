"
This is a specialized pluggable button morph that is meant to represent a tab in a set of tabs arranged horizontally.  Each tab will overlap slightly when drawn.  All but one tab will be drawn in left to right order in the specified color, but lighter.  The active tab will be drawn last in the full color and slightly taller to indicate that it is selected.  Clicking the active tab has no effect but clicking any other tab will change the active tab to the clicked tab.

This morph does not itself accept any events.  The parent tab set will grab the mouse clicks and handle notifying the appropriate tabs that they have been activated or deactivated.

There is a single selector which provides the text for the button label and affects the width of the tab.  When the width changes the tab will inform its parent that it has changed and that the layout needs to be updated.  The model for the text selector of course should be the client for the tab set.

The button label can be a String, Text, or Morph.  Texts work better than plain Strings.
"
Class {
	#name : #PluggableTabButtonMorph,
	#superclass : #Morph,
	#instVars : [
		'active',
		'model',
		'textSelector',
		'arcLengths',
		'subMorph'
	],
	#category : #Morphic-Widgets-Pluggable
}

{ #category : #'instance creation' }
PluggableTabButtonMorph class >> on: anObject label: getTextSelector [
	^self new
	      model: anObject;
		textSelector: getTextSelector;
		yourself.
]

{ #category : #access }
PluggableTabButtonMorph >> active [
	active ifNil: [ active := false ].
	^ active
]

{ #category : #access }
PluggableTabButtonMorph >> active: aBoolean [
	active := aBoolean.
	self changed.
]

{ #category : #'private - access' }
PluggableTabButtonMorph >> arcLengths [
	arcLengths ifNil: [ self calculateArcLengths ].
	^ arcLengths
]

{ #category : #'private - access' }
PluggableTabButtonMorph >> arcLengths: anArrayOfIntegers [
	arcLengths := anArrayOfIntegers

]

{ #category : #precalculations }
PluggableTabButtonMorph >> calculateArcLengths [
	| array radius |
	radius := self cornerRadius.
	array := Array new: radius.
	
	1 to: radius do: [ :i | | x |
		x := i - 0.5.
		array at: i
		 	put: (radius - ((2 * x * radius) - (x * x)) sqrt) asInteger].
		
	self arcLengths: array
]

{ #category : #'private - access' }
PluggableTabButtonMorph >> cornerRadius [
	^ 5

]

{ #category : #drawing }
PluggableTabButtonMorph >> drawOn: aCanvas [
	self drawTabOn: aCanvas.
	self drawSubMorphOn: aCanvas
]

{ #category : #drawing }
PluggableTabButtonMorph >> drawSubMorphOn: aCanvas [
	| morphBounds |
	morphBounds := self bounds insetBy: (self cornerRadius + 3) @ (self topInactiveGap // 2 + 2).
	morphBounds := morphBounds translateBy: 0@(self topInactiveGap // 2 + 1).
	self active ifTrue: [
		morphBounds := morphBounds translateBy: 0@((self topInactiveGap // 2 + 1) negated)].
	self subMorph bounds height < (morphBounds height)
		ifTrue: [
			morphBounds := morphBounds
				insetBy: 0@((morphBounds height - self subMorph bounds height) // 2)].
	self subMorph bounds width < (morphBounds width)
		ifTrue: [
			morphBounds := morphBounds
				insetBy: ((morphBounds width - self subMorph bounds width) // 2)@0].

	self subMorph bounds: morphBounds.			
	aCanvas drawMorph: self subMorph
]

{ #category : #drawing }
PluggableTabButtonMorph >> drawTabOn: aCanvas [
	| top myColor cornerRadius myArcLengths myBounds |
	cornerRadius := self cornerRadius.
	myBounds := self bounds.
	self active
		ifTrue: [ top := myBounds top.
			myColor := self color ]
		ifFalse: [ top := myBounds top + self topInactiveGap.
			myColor := self color whiter whiter ].
	aCanvas fillRectangle:
		((myBounds left + cornerRadius)
				@ (top + cornerRadius)
			corner: (myBounds right - cornerRadius)
						@ self bottom)
		color: myColor.
	aCanvas fillRectangle:
		((myBounds left + (cornerRadius * 2)) @ top
			corner: (myBounds right - (cornerRadius * 2))
				@ (top + cornerRadius))
		color: myColor.
	aCanvas fillOval:
		((myBounds left + self cornerRadius) @ top
			corner: (myBounds left + (self cornerRadius * 3))
				@ (top + (self cornerRadius * 2)))
		color: myColor.
	aCanvas fillOval:
		((myBounds right - (self cornerRadius * 3)) @ top
			corner: (myBounds right - self cornerRadius)
				@ (top + (self cornerRadius * 2)))
		color: myColor.

	myArcLengths := self arcLengths.
	1 to: myArcLengths size do: [ :i | | length |
		length := myArcLengths at: i.
		aCanvas line: (myBounds left + cornerRadius - i) @ (myBounds bottom - 1 )
			to: (myBounds left + cornerRadius - i) @ (myBounds bottom - length - 1)
			color: myColor.
		aCanvas line: (myBounds right - cornerRadius + i - 1) @ (myBounds bottom - 1)
			to: (myBounds right - cornerRadius + i - 1) @ (myBounds bottom - length - 1)
			color: myColor]
	

]

{ #category : #event }
PluggableTabButtonMorph >> handlesKeyboard: evt [
	"Yes, we do it here."
	
	^true
]

{ #category : #keymapping }
PluggableTabButtonMorph >> initializeShortcuts: aKMDispatcher [

	super initializeShortcuts: aKMDispatcher.
	aKMDispatcher attachCategory: #MorphFocusNavigation
]

{ #category : #access }
PluggableTabButtonMorph >> innerExtent: aPoint [
	"Set the extent based on the primary visible part of the tab.  In other words add twice the cornerRadius to this extent"
	self extent: (aPoint x + (self cornerRadius * 2)) @ (aPoint y)
]

{ #category : #event }
PluggableTabButtonMorph >> keyStroke: event [

	(self navigationKey: event) ifTrue: [^self].
	super keyStroke: event
]

{ #category : #access }
PluggableTabButtonMorph >> model [
	^ model

]

{ #category : #access }
PluggableTabButtonMorph >> model: anObject [
	model := anObject
]

{ #category : #access }
PluggableTabButtonMorph >> outerGap [
	"The horizontal distance of the outer left and right edges of the tab excluding the inner visible part"
	^ self cornerRadius * 2
]

{ #category : #stepping }
PluggableTabButtonMorph >> step [
	self subMorph step.
	self changed.

]

{ #category : #stepping }
PluggableTabButtonMorph >> stepTime [
	^ self subMorph stepTime

]

{ #category : #'private - access' }
PluggableTabButtonMorph >> subMorph [
	subMorph ifNil: [ self update: self textSelector ].
	^ subMorph
]

{ #category : #'private - access' }
PluggableTabButtonMorph >> subMorph: aMorph [
	subMorph := aMorph

]

{ #category : #event }
PluggableTabButtonMorph >> takesKeyboardFocus [
	"Answer whether the receiver can normally take keyboard focus."

	^true
]

{ #category : #access }
PluggableTabButtonMorph >> textSelector [
	^ textSelector

]

{ #category : #access }
PluggableTabButtonMorph >> textSelector: aSymbol [
	textSelector := aSymbol
]

{ #category : #actions }
PluggableTabButtonMorph >> toggle [
	self active: self active not
]

{ #category : #'private - access' }
PluggableTabButtonMorph >> topInactiveGap [
	^ 5
]

{ #category : #updating }
PluggableTabButtonMorph >> update: aSelector [
	self textSelector
		ifNotNil: [ 
			aSelector = self textSelector
				ifTrue: [ 
					| morph |
					morph := (aSelector isSymbol and: [ model notNil ])
						ifTrue: [ (self model perform: aSelector) asMorph ]
						ifFalse: [ aSelector value asMorph ].
					self subMorph: morph ] ].
	self changed
]

{ #category : #stepping }
PluggableTabButtonMorph >> wantsSteps [
	^ self subMorph wantsSteps
]
