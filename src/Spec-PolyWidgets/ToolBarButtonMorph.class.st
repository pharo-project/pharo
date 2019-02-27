"
A button morph typically used on a toolbar. The button can have a badge.
"
Class {
	#name : #ToolBarButtonMorph,
	#superclass : #PluggableButtonMorph,
	#instVars : [
		'badgeFont',
		'badgeSelector'
	],
	#category : #'Spec-PolyWidgets-ToolBar'
}

{ #category : #accessing }
ToolBarButtonMorph >> badge [
	badgeSelector ifNil: [ ^ nil ].
	^ self model perform: badgeSelector
]

{ #category : #accessing }
ToolBarButtonMorph >> badgeColor [
	^ Smalltalk ui theme badgeColor
]

{ #category : #accessing }
ToolBarButtonMorph >> badgeFont [
	^ badgeFont ifNil: [ badgeFont := self newBadgeFont ]
]

{ #category : #accessing }
ToolBarButtonMorph >> badgeSelector: aSymbol [
	badgeSelector := aSymbol
]

{ #category : #accessing }
ToolBarButtonMorph >> badgeTextColor [
	^ Smalltalk ui theme badgeTextColor
]

{ #category : #accessing }
ToolBarButtonMorph >> drawBadgeOn: aCanvas [
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

{ #category : #accessing }
ToolBarButtonMorph >> drawOn: aCanvas [
	super drawOn: aCanvas.
	self hasBadge
		ifTrue: [ self drawBadgeOn: aCanvas ]
]

{ #category : #accessing }
ToolBarButtonMorph >> getIconSelector [

	^ getIconSelector
]

{ #category : #accessing }
ToolBarButtonMorph >> getIconSelector: aSelector [

	getIconSelector := aSelector.
	self update: getIconSelector
]

{ #category : #accessing }
ToolBarButtonMorph >> getLabelSelector [

	^ getLabelSelector
]

{ #category : #accessing }
ToolBarButtonMorph >> getLabelSelector: aSelector [

	getLabelSelector := aSelector.
	self update: getLabelSelector
]

{ #category : #accessing }
ToolBarButtonMorph >> hasBadge [
	^ self badge notNil
]

{ #category : #accessing }
ToolBarButtonMorph >> newBadgeFont [
	| baseBadgeFont |
	baseBadgeFont := StandardFonts codeFont.
 	^ (LogicalFont 
		familyName: baseBadgeFont familyName 
		pointSize: baseBadgeFont pointSize - 3)
			weightValue: 700
]

{ #category : #accessing }
ToolBarButtonMorph >> normalBorderStyle [
	^ BorderStyle simple
		width: 0;
		baseColor: Color transparent
]

{ #category : #accessing }
ToolBarButtonMorph >> normalFillStyle [
	^ Color transparent
]
