Class {
	#name : #SpToolbarToggleButtonMorph,
	#superclass : #PluggableToggleButtonMorph,
	#instVars : [
		'badgeFont',
		'badgeSelector'
	],
	#category : #'Spec2-Adapters-Morphic'
}

{ #category : #accessing }
SpToolbarToggleButtonMorph >> badge [
	badgeSelector ifNil: [ ^ nil ].
	^ self model perform: badgeSelector
]

{ #category : #accessing }
SpToolbarToggleButtonMorph >> badgeColor [
	^ self theme badgeColor
]

{ #category : #accessing }
SpToolbarToggleButtonMorph >> badgeFont [
	^ badgeFont ifNil: [ badgeFont := self newBadgeFont ]
]

{ #category : #accessing }
SpToolbarToggleButtonMorph >> badgeSelector: aSymbol [
	badgeSelector := aSymbol
]

{ #category : #accessing }
SpToolbarToggleButtonMorph >> badgeTextColor [
	^ self theme badgeTextColor
]

{ #category : #configuring }
SpToolbarToggleButtonMorph >> configureWith: displayModel item: itemPresenter [
	displayModel configureButton: self item: itemPresenter
]

{ #category : #accessing }
SpToolbarToggleButtonMorph >> drawBadgeOn: aCanvas [
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
SpToolbarToggleButtonMorph >> drawOn: aCanvas [
	super drawOn: aCanvas.
	self hasBadge ifTrue: [
		self drawBadgeOn: aCanvas ]
]

{ #category : #accessing }
SpToolbarToggleButtonMorph >> drawSubmorphsOn: aCanvas [
	
	super drawSubmorphsOn: aCanvas
]

{ #category : #accessing }
SpToolbarToggleButtonMorph >> getIconSelector [

	^ getIconSelector
]

{ #category : #accessing }
SpToolbarToggleButtonMorph >> getIconSelector: aSelector [

	getIconSelector := aSelector.
	self update: getIconSelector
]

{ #category : #accessing }
SpToolbarToggleButtonMorph >> getLabelSelector [

	^ getLabelSelector
]

{ #category : #accessing }
SpToolbarToggleButtonMorph >> getLabelSelector: aSelector [

	getLabelSelector := aSelector.
	self update: getLabelSelector
]

{ #category : #accessing }
SpToolbarToggleButtonMorph >> hasBadge [
	^ self badge notNil
]

{ #category : #style }
SpToolbarToggleButtonMorph >> mouseOverBorderStyle [
	^ BorderStyle simple
		width: 0;
		baseColor: Color transparent
]

{ #category : #accessing }
SpToolbarToggleButtonMorph >> newBadgeFont [
	| baseBadgeFont |
	baseBadgeFont := StandardFonts codeFont.
 	^ (LogicalFont 
		familyName: baseBadgeFont familyName 
		pointSize: baseBadgeFont pointSize - 3)
			weightValue: 700
]

{ #category : #style }
SpToolbarToggleButtonMorph >> normalBorderStyle [
	^ BorderStyle simple
		width: 0;
		baseColor: Color transparent
]

{ #category : #style }
SpToolbarToggleButtonMorph >> normalFillStyle [
	^ Color transparent
]
