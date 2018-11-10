"
I implement a toolbar button.

I do not have border or insets.
I can show a badge.

I'm used with IceTipToolbarItem
"
Class {
	#name : #IceTipToolbarItemButtonMorph,
	#superclass : #PluggableButtonMorph,
	#instVars : [
		'badgeFont',
		'badgeSelector'
	],
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #accessing }
IceTipToolbarItemButtonMorph >> badge [
	badgeSelector ifNil: [ ^ nil ].
	^ self model perform: badgeSelector
]

{ #category : #accessing }
IceTipToolbarItemButtonMorph >> badgeColor [
	^ Smalltalk ui theme badgeColor
]

{ #category : #accessing }
IceTipToolbarItemButtonMorph >> badgeFont [
	^ badgeFont ifNil: [ badgeFont := self newBadgeFont ]
]

{ #category : #accessing }
IceTipToolbarItemButtonMorph >> badgeSelector: aSymbol [
	badgeSelector := aSymbol
]

{ #category : #accessing }
IceTipToolbarItemButtonMorph >> badgeTextColor [
	^ Smalltalk ui theme badgeTextColor
]

{ #category : #drawing }
IceTipToolbarItemButtonMorph >> drawBadgeOn: aCanvas [
	| badgeString badgeBounds textBounds textWidth textHeight width |
	
	badgeString := self badge asString.
	textWidth := self badgeFont widthOfString: badgeString.
	textHeight := self badgeFont height.
	width := 14 max: textWidth + 2. 
	badgeBounds := ((bounds right - width - 2)@(bounds top + 2) extent: width@14).
	aCanvas 
		fillOval: badgeBounds
		fillStyle: self badgeColor.
		
	textBounds := 
		(badgeBounds center - ((textWidth@textHeight) / 2.0)) + (1@0) 
		extent: textWidth@textHeight. 
	aCanvas 
		drawString: badgeString
		in: textBounds
		font: self badgeFont 
		color: self badgeTextColor
]

{ #category : #drawing }
IceTipToolbarItemButtonMorph >> drawOn: aCanvas [
	super drawOn: aCanvas.
	self hasBadge
		ifTrue: [ self drawBadgeOn: aCanvas ]
]

{ #category : #testing }
IceTipToolbarItemButtonMorph >> hasBadge [
	^ self badge notNil
]

{ #category : #drawing }
IceTipToolbarItemButtonMorph >> newBadgeFont [
	| baseBadgeFont |
	baseBadgeFont := StandardFonts codeFont.
 	^ (LogicalFont 
		familyName: baseBadgeFont familyName 
		pointSize: baseBadgeFont pointSize - 3)
			weightValue: 700
]

{ #category : #style }
IceTipToolbarItemButtonMorph >> normalBorderStyle [
	^ BorderStyle simple
		width: 0;
		baseColor: Color transparent
]

{ #category : #style }
IceTipToolbarItemButtonMorph >> normalFillStyle [
	^ Color transparent
]
