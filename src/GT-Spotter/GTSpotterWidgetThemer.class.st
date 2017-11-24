"
This is a themer for all spotter morphs.

To customize it, it has to be subclassed and pointed to from:
ACustomTheme>>spotterWidgetThemer
"
Class {
	#name : #GTSpotterWidgetThemer,
	#superclass : #GLMBrickThemerModule,
	#category : #'GT-Spotter-Theme'
}

{ #category : #values }
GTSpotterWidgetThemer >> backgroundColor [
	"used as background color for all panes"
	^ Color white
]

{ #category : #values }
GTSpotterWidgetThemer >> borderColor [
	"color that is used for border and dividers of UI parts of Spotter
	for example divider between header and results or between preview and list"
	^ Color r: 180 g: 180 b: 180 range: 255
]

{ #category : #values }
GTSpotterWidgetThemer >> breadcrumbHeight [

	^ 26
]

{ #category : #values }
GTSpotterWidgetThemer >> breadcrumbItemBackgroundColor [

	^ self borderColor
]

{ #category : #styles }
GTSpotterWidgetThemer >> breadcrumbItemLabelStyleFor: aBrick [

	aBrick
		textPosition: #center;
		padding: { 0 . self outerBorderWidth };
		vSpaceFill;
		hSpaceFill
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> breadcrumbItemLabelWidgetStyleFor: aBrick [

	aBrick
		color: self breadcrumbItemBackgroundColor;
		textColor: self breadcrumbItemTextColor
]

{ #category : #styles }
GTSpotterWidgetThemer >> breadcrumbItemStyleFor: aBrick [

	aBrick vSpaceFill
]

{ #category : #values }
GTSpotterWidgetThemer >> breadcrumbItemTextColor [

	^ Color white
]

{ #category : #styles }
GTSpotterWidgetThemer >> breadcrumbItemTriangleStyleFor: aBrick [

	aBrick
		height: self breadcrumbTriangleHeight;
		hSpaceFill
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> breadcrumbItemTriangleWidgetStyleFor: aBrick [

	aBrick
		renderer: (GTSpotterTriangleRenderer new orientation: #bottom).
		
	aBrick owner isTriangleVisible
		ifTrue:[ aBrick color: self breadcrumbItemBackgroundColor ]
		ifFalse: [ aBrick color: Color transparent ]
]

{ #category : #values }
GTSpotterWidgetThemer >> breadcrumbSeparatorColor [

	^ self breadcrumbItemBackgroundColor muchLighter slightlyDarker
]

{ #category : #values }
GTSpotterWidgetThemer >> breadcrumbSeparatorLineWidth [

	^ 2
]

{ #category : #styles }
GTSpotterWidgetThemer >> breadcrumbSeparatorStyleFor: aBrick [

	aBrick
		borderWidth: self breadcrumbSeparatorLineWidth;
		width: self breadcrumbSeparatorWidth;
		height: self breadcrumbHeight + self outerBorderWidth.
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> breadcrumbSeparatorWidgetStyleFor: aBrick [

	aBrick
		color: self breadcrumbItemBackgroundColor;
		borderColor: self breadcrumbSeparatorColor;
		renderer: GTSpotterBreadcrumbSeparatorRenderer new
]

{ #category : #values }
GTSpotterWidgetThemer >> breadcrumbSeparatorWidth [

	^ (self breadcrumbHeight + self outerBorderWidth) / 2 + 4 "we need slightly more because of drawing accuracy"
]

{ #category : #styles }
GTSpotterWidgetThemer >> breadcrumbStyleFor: aBrick [
	
	aBrick
		margin: { 0 . 0 };
		marginBottom: self breadcrumbTriangleHeight negated;
		height: self breadcrumbHeight + self breadcrumbTriangleHeight.
		
	aBrick layout
		zIndex: 2.
]

{ #category : #values }
GTSpotterWidgetThemer >> breadcrumbTriangleHeight [

	^ 16
]

{ #category : #values }
GTSpotterWidgetThemer >> buttonPressedColor [

	^ self backgroundColor darker
]

{ #category : #styles }
GTSpotterWidgetThemer >> categoryAmountStyleFor: aBrick [

	aBrick
		textPosition: #leftCenter.
	
	aBrick label
		familyName: self titleFamilyName.
	
	aBrick
		vSpaceFill;
		width: #approximateWidth;
		marginLeft: self categoryNameMarginRight.
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> categoryAmountWidgetStyleFor: aBrick [

	aBrick
		textColor: self titleTextColor;
		color: self titleBackgroundColor
]

{ #category : #values }
GTSpotterWidgetThemer >> categoryMarginBottom [

	^ (self itemHeight * (2/3)) ceiling "pixels"
]

{ #category : #values }
GTSpotterWidgetThemer >> categoryNameMarginLeft [

	^ self itemIconMaxWidth
]

{ #category : #values }
GTSpotterWidgetThemer >> categoryNameMarginRight [

	^ 10
]

{ #category : #styles }
GTSpotterWidgetThemer >> categoryStyleFor: aBrick [

	aBrick
		marginBottom: self categoryMarginBottom;
		hSpaceFill
]

{ #category : #values }
GTSpotterWidgetThemer >> categoryTitleBackgroundColor [

	^ self backgroundColor alpha: 0.95
]

{ #category : #styles }
GTSpotterWidgetThemer >> categoryTitleNameStyleFor: aBrick [

	aBrick
		vSpaceFill;
		textPosition: #leftCenter;
		width: [ :name | (aBrick owner innerBounds width) min: ((name label textSize x) + name label hMargin) ].
	
	aBrick label
		familyName: self titleFamilyName.
	
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> categoryTitleNameWidgetStyleFor: aBrick [

	aBrick
		textColor: self titleTextColor;
		color: self titleBackgroundColor
	
]

{ #category : #styles }
GTSpotterWidgetThemer >> categoryTitleShortcutStyleFor: aBrick [

	aBrick
		vSpaceFill;
		textPosition: #leftCenter;
		textColor: self titleTextColor.
	
	aBrick label
		familyName: 'Lucida Grande'.
	
]

{ #category : #styles }
GTSpotterWidgetThemer >> categoryTitleStyleFor: aBrick [

	aBrick
		useHorizontalLinearLayout;
		height: 30;
		hSpaceFill;
		paddingLeft: self categoryNameMarginLeft
	
	"aBrick layout
		floating: #stack;
		maxWidth: [ :title |
			aBrick hPadding + 
			(title nameBrick label textSize x )+ title amountBrick label textSize x +
			title nameBrick hPadding + title nameBrick hMargin + 
			title nameBrick label hPadding + title nameBrick label hMargin + 
			title amountBrick hMargin ]"
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> categoryTitleWidgetStyleFor: aBrick [

	aBrick
		normalColor: self categoryTitleBackgroundColor
]

{ #category : #styles }
GTSpotterWidgetThemer >> closeButtonStyleFor: aBrick [

	aBrick
		vSpaceFill;
		width: [ :m | m height ];
		margin: 10;
		marginLeft: 0
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> closeButtonWidgetStyleFor: aBrick [

	aBrick
		normalImage: GLMUIThemeExtraIcons glamorousClose;
		pressedColor: self buttonPressedColor
]

{ #category : #styles }
GTSpotterWidgetThemer >> dropDownPaneStyleFor: aBrick [

	aBrick
		layoutProperties: GLMBrickLayoutProperties new;
		marginTop: self innerBorderWidth;
		vSpaceFill;
		hSpaceFill
]

{ #category : #values }
GTSpotterWidgetThemer >> headerHeight [
	"height of the header where user enters search request"
	^ 56
]

{ #category : #styles }
GTSpotterWidgetThemer >> headerStyleFor: aBrick [

	aBrick
		hSpaceFill;
		height: self headerHeight
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> headerWidgetStyleFor: aBrick [

	aBrick
		color: self backgroundColor
]

{ #category : #styles }
GTSpotterWidgetThemer >> helpButtonStyleFor: aBrick [

	aBrick
		vSpaceFill;
		width: [ :m | m height ];
		margin: 10;
		marginLeft: 0
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> helpButtonWidgetStyleFor: aBrick [

	aBrick
		normalImage: GLMUIThemeExtraIcons glamorousHelp;
		pressedColor: self buttonPressedColor
]

{ #category : #styles }
GTSpotterWidgetThemer >> hintHideArrowStyleFor: aBrick [

	aBrick ifNil: [ ^ self ].
	(aBrick valueOfProperty: #brickSpotterIsReveal ifAbsent: [ false ])
		ifFalse: [ ^ self ].
	aBrick setProperty: #brickSpotterIsReveal toValue: false.
	
	(aBrick hasProperty: #brickSpotterBackupColor)
		ifTrue: [ aBrick color: (aBrick valueOfProperty: #brickSpotterBackupColor) ].
		
	aBrick changed
]

{ #category : #styles }
GTSpotterWidgetThemer >> hintHideStyleFor: aBrick [

	aBrick ifNil: [ ^ self ].
	(aBrick valueOfProperty: #brickSpotterIsReveal ifAbsent: [ false ])
		ifFalse: [ ^ self ].
	aBrick setProperty: #brickSpotterIsReveal toValue: false.
	
	(aBrick hasProperty: #brickSpotterBackupColor)
		ifTrue: [ aBrick color: (aBrick valueOfProperty: #brickSpotterBackupColor) ].
		
	(aBrick hasProperty: #brickSpotterBackupBorder)
		ifTrue: [ aBrick border: (aBrick valueOfProperty: #brickSpotterBackupBorder) ].
		
	aBrick changed
]

{ #category : #styles }
GTSpotterWidgetThemer >> hintRevealArrowStyleFor: aBrick [
	
	SmalltalkImage current vm canHandleShiftPressed
		ifTrue: [ ^ self ].
	
	aBrick ifNil: [ ^ self ].
	(aBrick valueOfProperty: #brickSpotterIsReveal ifAbsent: [ false ])
		ifTrue: [ ^ self ].
	
	aBrick setProperty: #brickSpotterBackupColor toValue: aBrick color.
	aBrick setProperty: #brickSpotterIsReveal toValue: true.
	
	aBrick color: self themer popupHelpThemer backgroundColor.
	
	aBrick changed
]

{ #category : #styles }
GTSpotterWidgetThemer >> hintRevealStyleFor: aBrick [
	
	SmalltalkImage current vm canHandleShiftPressed
		ifTrue: [ ^ self ].
	
	aBrick ifNil: [ ^ self ].
	(aBrick valueOfProperty: #brickSpotterIsReveal ifAbsent: [ false ])
		ifTrue: [ ^ self ].
	
	aBrick setProperty: #brickSpotterBackupColor toValue: aBrick color.
	aBrick setProperty: #brickSpotterBackupBorder toValue: aBrick border.
	aBrick setProperty: #brickSpotterIsReveal toValue: true.
	
	aBrick color: self themer popupHelpThemer backgroundColor.
	aBrick border: GLMBrickBorder new.
	aBrick borderColor: (self themer popupHelpThemer borderColor alpha: 0.6).
	aBrick borderWidth: self themer popupHelpThemer borderWidth.
	
	aBrick changed
]

{ #category : #icons }
GTSpotterWidgetThemer >> iconAct [
	^ self iconNamed: #glamorousAccept
]

{ #category : #icons }
GTSpotterWidgetThemer >> iconDiveIn [
	^ self iconNamed: #glamorousDiveRightIcon
]

{ #category : #icons }
GTSpotterWidgetThemer >> iconDiveInCategory [
	^ self iconNamed: #glamorousDiveAllRightIcon
]

{ #category : #values }
GTSpotterWidgetThemer >> innerBorderWidth [
	"width of the border that separates panes in the spotter"
	^ 1
]

{ #category : #styles }
GTSpotterWidgetThemer >> itemActionButtonStyleFor: aBrick [

	aBrick
		width: [ (self itemHeight / 1.1) floor ];
		vSpaceFill;
		margin: #(0 1);
		padding: { 0 . self outerBorderWidth }
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> itemActionButtonWidgetStyleFor: aBrick [

	aBrick
		pressedColor: self itemActionSelectedBackgroundColor
]

{ #category : #values }
GTSpotterWidgetThemer >> itemActionSelectedBackgroundColor [

	^ self itemSelectedBackgroundColor darker
]

{ #category : #styles }
GTSpotterWidgetThemer >> itemActionbarStyleFor: aBrick [

	aBrick
		useHorizontalLinearLayout;
		hShrinkWrap;
		vSpaceFill;
		marginRight: self outerBorderWidth + 2;
		visible: false
]

{ #category : #values }
GTSpotterWidgetThemer >> itemHeight [

	^  16 + Smalltalk ui theme labelFont height
]

{ #category : #values }
GTSpotterWidgetThemer >> itemHoveredBackgroundColor [

	^ self itemSelectedBackgroundColor alpha: 0.35
]

{ #category : #values }
GTSpotterWidgetThemer >> itemIconAlphaValue [

	^ 0.8
]

{ #category : #values }
GTSpotterWidgetThemer >> itemIconMaxWidth [

	^ 50
]

{ #category : #styles }
GTSpotterWidgetThemer >> itemIconStyleFor: aBrick [
	"icon form must already be set before styling"

	self flag: 'figure out why alpha doesn''t work'.
	aBrick
		"making icon a bit transparent"
		"normalImage: (aBrick normalImage asAlphaImageMorph alpha: self itemIconAlphaValue) imageForm;"
		width: self itemIconMaxWidth;
		vSpaceFill
]

{ #category : #styles }
GTSpotterWidgetThemer >> itemLabelStyleFor: aBrick [

	aBrick label
		highlightingLogic: [ :aCanvas :aRectangle | |bounds|
			"because font calculates text width wrong"
			bounds := aRectangle translateBy: (-3@0).
			aCanvas line: bounds bottomLeft to: bounds bottomRight width: 1 color: Color lightGray ].
	
	aBrick
		hSpaceFill;
		vSpaceFill;
		marginRight: self outerBorderWidth.
]

{ #category : #values }
GTSpotterWidgetThemer >> itemNormalBackgroundColor [

	^ Color transparent
]

{ #category : #styles }
GTSpotterWidgetThemer >> itemNormalStyleFor: aBrick [

	aBrick
		height: [ self itemHeight ];
		hSpaceFill
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> itemNormalWidgetStyleFor: aBrick [

	aBrick
		normalColor: self itemNormalBackgroundColor;
		selectedColor: self itemHoveredBackgroundColor;
		checkedColor: self itemSelectedBackgroundColor
]

{ #category : #values }
GTSpotterWidgetThemer >> itemSelectedBackgroundColor [

	^ self backgroundColor darker
]

{ #category : #values }
GTSpotterWidgetThemer >> outerBorderWidth [
	"the width of the border around the whole spotter"
	^ 4
]

{ #category : #values }
GTSpotterWidgetThemer >> previewArrowColor [

	^ self itemSelectedBackgroundColor
]

{ #category : #values }
GTSpotterWidgetThemer >> previewArrowGapHack [

	^ self themer scrollbarOffset + self themer scrollbarWidth + self outerBorderWidth + 1"pager bug"
]

{ #category : #values }
GTSpotterWidgetThemer >> previewArrowWidth [

	^ (self itemHeight / 2) + self previewArrowGapHack
]

{ #category : #values }
GTSpotterWidgetThemer >> previewBorderWidth [

	^ self outerBorderWidth
]

{ #category : #values }
GTSpotterWidgetThemer >> previewPaddingLeft [

	^ self previewArrowWidth * 1.2
]

{ #category : #values }
GTSpotterWidgetThemer >> previewPaddingRight [

	^ self previewPaddingLeft


]

{ #category : #values }
GTSpotterWidgetThemer >> previewWidthPercent [

	^ 40
]

{ #category : #values }
GTSpotterWidgetThemer >> privacyAgreementIsNotSendingDataColor [

	^ Color paleRed
]

{ #category : #values }
GTSpotterWidgetThemer >> privacyAgreementIsSendingDataColor [

	^ Color paleGreen
]

{ #category : #styles }
GTSpotterWidgetThemer >> privacyAgreementNotificationStyleFor: aBrick [

	| color |
	color := aBrick sendUsageData 
		ifTrue: [ self privacyAgreementIsSendingDataColor. ]
		ifFalse: [ self privacyAgreementIsNotSendingDataColor. ].
	aBrick messageBrick rubricTextFamilyName: StandardFonts defaultFont familyName.
	aBrick messageBrick rubricTextColor: Color black.
	aBrick
		color: color;
		borderColor: color darker;
		borderWidth: self outerBorderWidth;
		marginTop: 0
]

{ #category : #styles }
GTSpotterWidgetThemer >> resultsPaneStyleFor: aBrick [

	aBrick
		hSpaceFill: self resultsPaneWidth;
		vSpaceFill;
		with: [ :b | b band layout: GTSpotterStackedLayout new ]
]

{ #category : #values }
GTSpotterWidgetThemer >> resultsPaneWidth [
	"width in Percents of the results pane"
	^ 100"%"
]

{ #category : #values }
GTSpotterWidgetThemer >> searchFieldFamilyName [

	^ 'Source Sans Pro'
]

{ #category : #values }
GTSpotterWidgetThemer >> searchFieldFontSize [

	^ 15
]

{ #category : #values }
GTSpotterWidgetThemer >> searchFieldMarginLeft [

	^ 20
]

{ #category : #styles }
GTSpotterWidgetThemer >> searchFieldStyleFor: aBrick [
	
	aBrick
		beCentered;
		hSpaceFill;
		vSpaceFill;
		withoutMargins;
		withoutAdornment;
		withoutBorder;
		beTransparent;
		withGhostText: self spotterGhostText;
		autoAccept: true;
		marginLeft: self searchFieldMarginLeft;
		rubricTextFamilyName: self searchFieldFamilyName;
		rubricTextFontSize: self searchFieldFontSize.
	
	aBrick rubric ghostTextRuler
		fontName: self searchFieldFamilyName pointSize: self searchFieldFontSize.
]

{ #category : #values }
GTSpotterWidgetThemer >> searchFieldTextColor [
	
	^ Color black
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> searchFieldWidgetStyleFor: aBrick [
	
	aBrick morph
		color: Color transparent;
		textColor: self searchFieldTextColor;
		borderColor: Color transparent.
		
	aBrick morph textArea
		backgroundColor: Color transparent
]

{ #category : #styles }
GTSpotterWidgetThemer >> settingsButtonStyleFor: aBrick [

	aBrick
		vSpaceFill;
		width: [ :m | m height ];
		margin: #(10 0);
		marginRight: 2
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> settingsButtonWidgetStyleFor: aBrick [
	aBrick
		normalImage: (Smalltalk ui icons iconNamed: #glamorousPreferences);
		pressedColor: self buttonPressedColor
]

{ #category : #values }
GTSpotterWidgetThemer >> settingsLabelPadding [

	^ 4
]

{ #category : #values }
GTSpotterWidgetThemer >> settingsLabelSelectedTextColor [

	^ Color black
]

{ #category : #values }
GTSpotterWidgetThemer >> settingsLabelTextColor [

	^ Color white
]

{ #category : #styles }
GTSpotterWidgetThemer >> spotterContentsStyleFor: aBrick [
	
	aBrick
		useVerticalLinearLayout;
		borderWidth: self outerBorderWidth;
		padding: self outerBorderWidth;
		hSpaceFill;
		vSpaceFill.
	
	aBrick layout
		zIndex: 2
	
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> spotterContentsWidgetStyleFor: aBrick [
	
	aBrick
		borderColor: self borderColor;
		color: self borderColor
	
]

{ #category : #values }
GTSpotterWidgetThemer >> spotterGhostText [

	^ 'Search'
]

{ #category : #styles }
GTSpotterWidgetThemer >> spotterPaneStyleFor: aBrick [
	
	aBrick
		useHorizontalLinearLayout;
		hSpaceFill;
		vSpaceFill
]

{ #category : #styles }
GTSpotterWidgetThemer >> spotterPreviewArrowContainerStyleFor: aBrick [

	aBrick
		useVerticalLinearLayout;
		vSpaceFill;
		width: self previewArrowWidth;
		margin: {
			self headerHeight + self innerBorderWidth + self previewBorderWidth.
			0.
			self previewBorderWidth.
			self previewArrowGapHack negated }.
		
	aBrick layout
		zIndex: 4.
]

{ #category : #styles }
GTSpotterWidgetThemer >> spotterPreviewArrowStyleFor: aBrick [

	aBrick
		hSpaceFill;
		renderer: (GTSpotterTriangleRenderer new orientation: #right);
		height: [ (self previewArrowWidth - self previewArrowGapHack ) * 2 ]
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> spotterPreviewArrowWidgetStyleFor: aBrick [

	aBrick owner isTriangleVisible
		ifTrue: [
			aBrick color: self previewArrowColor.
			aBrick renderer borderColor: self borderColor ]
		ifFalse: [
			aBrick color: Color transparent.
			aBrick renderer borderColor: Color transparent ].
	
	GTSpotter isPreviewVisible
		ifTrue: [ 
			aBrick selectedColor: self borderColor.
			aBrick renderer border: 0 ]
		ifFalse: [
			aBrick selectedColor: Color transparent.
			aBrick renderer border: self outerBorderWidth ]
]

{ #category : #styles }
GTSpotterWidgetThemer >> spotterPreviewCompositeStyleFor: aBrick [

	aBrick
		hSpaceFill;
		vSpaceFill;
		margin: self previewBorderWidth;
		paddingLeft: self previewPaddingLeft
]

{ #category : #styles }
GTSpotterWidgetThemer >> spotterPreviewStyleFor: aBrick [
	
	aBrick
		borderWidth: self previewBorderWidth;
		hSpaceFill: self previewWidthPercent;
		vSpaceFill;
		margin: { self headerHeight + self innerBorderWidth. 0 . 0};
		marginLeft: (self previewBorderWidth + self previewArrowWidth) negated.
	
	aBrick layout
		zIndex: 1
]

{ #category : #'styles-widget' }
GTSpotterWidgetThemer >> spotterPreviewWidgetStyleFor: aBrick [
	
	aBrick
		borderColor: self borderColor;
		color: self backgroundColor
]

{ #category : #styles }
GTSpotterWidgetThemer >> spotterStyleFor: aBrick [

	aBrick brickRoot useVerticalLinearLayout
]

{ #category : #values }
GTSpotterWidgetThemer >> titleBackgroundColor [

	^ Color transparent
]

{ #category : #values }
GTSpotterWidgetThemer >> titleFamilyName [
	^ 'Open Sans'.
]

{ #category : #values }
GTSpotterWidgetThemer >> titleMarginLeft [
	^ 20
]

{ #category : #values }
GTSpotterWidgetThemer >> titleTextColor [

	^ self themer textLightColor
]
