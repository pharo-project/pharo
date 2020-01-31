"
A button morph typically used on a toolbar. The button can have a badge.
"
Class {
	#name : #SpToolBarButtonMorph,
	#superclass : #PluggableButtonMorph,
	#instVars : [
		'badgeFont',
		'badgeSelector'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #accessing }
SpToolBarButtonMorph >> badge [
	badgeSelector ifNil: [ ^ nil ].
	^ self model perform: badgeSelector
]

{ #category : #accessing }
SpToolBarButtonMorph >> badgeColor [
	^ self theme badgeColor
]

{ #category : #accessing }
SpToolBarButtonMorph >> badgeFont [
	^ badgeFont ifNil: [ badgeFont := self newBadgeFont ]
]

{ #category : #accessing }
SpToolBarButtonMorph >> badgeSelector: aSymbol [
	badgeSelector := aSymbol
]

{ #category : #accessing }
SpToolBarButtonMorph >> badgeTextColor [
	^ self theme badgeTextColor
]

{ #category : #configuring }
SpToolBarButtonMorph >> configureWith: displayModel item: itemPresenter [
	displayModel configureButton: self item: itemPresenter
]

{ #category : #accessing }
SpToolBarButtonMorph >> drawBadgeOn: aCanvas [
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
SpToolBarButtonMorph >> drawOn: aCanvas [
	super drawOn: aCanvas.
	self hasBadge
		ifTrue: [ self drawBadgeOn: aCanvas ]
]

{ #category : #accessing }
SpToolBarButtonMorph >> getIconSelector [

	^ getIconSelector
]

{ #category : #accessing }
SpToolBarButtonMorph >> getIconSelector: aSelector [

	getIconSelector := aSelector.
	self update: getIconSelector
]

{ #category : #accessing }
SpToolBarButtonMorph >> getLabelSelector [

	^ getLabelSelector
]

{ #category : #accessing }
SpToolBarButtonMorph >> getLabelSelector: aSelector [

	getLabelSelector := aSelector.
	self update: getLabelSelector
]

{ #category : #accessing }
SpToolBarButtonMorph >> hasBadge [
	^ self badge notNil
]

{ #category : #accessing }
SpToolBarButtonMorph >> newBadgeFont [
	| baseBadgeFont |
	baseBadgeFont := StandardFonts codeFont.
 	^ (LogicalFont 
		familyName: baseBadgeFont familyName 
		pointSize: baseBadgeFont pointSize - 3)
			weightValue: 700
]

{ #category : #accessing }
SpToolBarButtonMorph >> normalBorderStyle [
	^ BorderStyle simple
		width: 0;
		baseColor: Color transparent
]

{ #category : #accessing }
SpToolBarButtonMorph >> normalFillStyle [
	^ Color transparent
]
