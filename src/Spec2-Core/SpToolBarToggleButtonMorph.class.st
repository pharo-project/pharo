Class {
	#name : #SpToolBarToggleButtonMorph,
	#superclass : #PluggableToggleButtonMorph,
	#instVars : [
		'badgeFont',
		'badgeSelector'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #accessing }
SpToolBarToggleButtonMorph >> badge [
	badgeSelector ifNil: [ ^ nil ].
	^ self model perform: badgeSelector
]

{ #category : #accessing }
SpToolBarToggleButtonMorph >> badgeColor [
	^ self theme badgeColor
]

{ #category : #accessing }
SpToolBarToggleButtonMorph >> badgeFont [
	^ badgeFont ifNil: [ badgeFont := self newBadgeFont ]
]

{ #category : #accessing }
SpToolBarToggleButtonMorph >> badgeSelector: aSymbol [
	badgeSelector := aSymbol
]

{ #category : #accessing }
SpToolBarToggleButtonMorph >> badgeTextColor [
	^ self theme badgeTextColor
]

{ #category : #configuring }
SpToolBarToggleButtonMorph >> configureWith: displayModel item: itemPresenter [
	displayModel configureButton: self item: itemPresenter
]

{ #category : #accessing }
SpToolBarToggleButtonMorph >> drawBadgeOn: aCanvas [
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
SpToolBarToggleButtonMorph >> drawOn: aCanvas [
	super drawOn: aCanvas.
	self hasBadge
		ifTrue: [ self drawBadgeOn: aCanvas ]
]

{ #category : #accessing }
SpToolBarToggleButtonMorph >> getIconSelector [

	^ getIconSelector
]

{ #category : #accessing }
SpToolBarToggleButtonMorph >> getIconSelector: aSelector [

	getIconSelector := aSelector.
	self update: getIconSelector
]

{ #category : #accessing }
SpToolBarToggleButtonMorph >> getLabelSelector [

	^ getLabelSelector
]

{ #category : #accessing }
SpToolBarToggleButtonMorph >> getLabelSelector: aSelector [

	getLabelSelector := aSelector.
	self update: getLabelSelector
]

{ #category : #accessing }
SpToolBarToggleButtonMorph >> hasBadge [
	^ self badge notNil
]

{ #category : #accessing }
SpToolBarToggleButtonMorph >> newBadgeFont [
	| baseBadgeFont |
	baseBadgeFont := StandardFonts codeFont.
 	^ (LogicalFont 
		familyName: baseBadgeFont familyName 
		pointSize: baseBadgeFont pointSize - 3)
			weightValue: 700
]

{ #category : #accessing }
SpToolBarToggleButtonMorph >> normalBorderStyle [
	^ BorderStyle simple
		width: 0;
		baseColor: Color transparent
]

{ #category : #accessing }
SpToolBarToggleButtonMorph >> normalFillStyle [
	^ Color transparent
]
