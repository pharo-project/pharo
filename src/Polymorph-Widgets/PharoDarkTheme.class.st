"
A dark theme for pharo, inspired on 

http://blog.jetbrains.com/blog/2013/06/11/inside-darcula-look-and-feel-an-interview-with-konstantin-bulenkov/

This theme works well with the ""Dark"" text styling. 
You can have ""the full experience"", executing: 

self installFullTheme.
"
Class {
	#name : #PharoDarkTheme,
	#superclass : #UITheme,
	#category : #'Polymorph-Widgets-Themes'
}

{ #category : #accessing }
PharoDarkTheme class >> baseColor [
	^ Color veryDarkGray
]

{ #category : #accessing }
PharoDarkTheme class >> basePassiveBackgroundColor [
	^ Color darkGray
]

{ #category : #accessing }
PharoDarkTheme class >> baseSelectionColor [
	^ Color r: 97 g: 163 b: 225 range: 255
]

{ #category : #accessing }
PharoDarkTheme class >> darkBaseColor [
	^ Color veryDarkGray
]

{ #category : #accessing }
PharoDarkTheme class >> isAbstract [
	"Answer whether the receiver is considered to be abstract."

	^false
]

{ #category : #accessing }
PharoDarkTheme class >> lightBaseColor [
	^ Color darkGray
]

{ #category : #accessing }
PharoDarkTheme class >> lightSelectionColor [
	^ Color r: 175 g: 213 b: 250 range: 255
]

{ #category : #settings }
PharoDarkTheme class >> setPreferredPreferences [
	"NECPreferences 
		expandPrefixes: true;
		popupShowWithShortcut: Character tab asShortcut."
]

{ #category : #accessing }
PharoDarkTheme class >> themeName [
	^ 'Pharo Dark'
]

{ #category : #accessing }
PharoDarkTheme class >> veryLightSelectionColor [
	^ self lightSelectionColor muchLighter
]

{ #category : #'accessing colors' }
PharoDarkTheme >> backgroundColor [
	^ Color r: 0.130 g: 0.130 b: 0.130
]

{ #category : #'accessing colors' }
PharoDarkTheme >> balloonBackgroundColor [

	^ self darkBaseColor lighter
]

{ #category : #'accessing colors' }
PharoDarkTheme >> borderColor [ 
	^ Color darkGray darker darker darker
]

{ #category : #'accessing colors' }
PharoDarkTheme >> buttonColor [
	^ self backgroundColor lighter lighter
]

{ #category : #'border-styles-buttons' }
PharoDarkTheme >> buttonNormalBorderStyleFor: aButton [
	"Return the normal button borderStyle for the given button."
	
	(aButton valueOfProperty: #noBorder ifAbsent: [ false ]) 
		ifTrue: [ 
			^ SimpleBorder new 
				width: 0; 
				baseColor: Color transparent ].
	
	^ SimpleBorder new
		width: 1;
		baseColor: self buttonColor lighter
]

{ #category : #'fill-styles-buttons' }
PharoDarkTheme >> buttonNormalFillStyleFor: aButton [
	"Return the normal button fillStyle for the given button."
	
	(aButton valueOfProperty: #noFill ifAbsent: [false]) 
		ifTrue: [ ^ SolidFillStyle color: Color transparent ].
		
	^ SolidFillStyle color: self buttonColor.
]

{ #category : #'fill-styles-buttons' }
PharoDarkTheme >> buttonSelectedFillStyleFor: aButton [
	"Return the normal button fillStyle for the given button."
	
	^ SolidFillStyle color: self selectionColor.
	"| top bottom |
	
	top := self secondarySelectionColor.
	bottom := self selectionColor.

	^(GradientFillStyle ramp: {
			0.0->top.
			0.7->bottom.})
		origin: aButton bounds origin;
		direction: 0 @ aButton height;
		radial: false"
]

{ #category : #'accessing colors' }
PharoDarkTheme >> caretColor [
	^ Color lightBlue.
]

{ #category : #'accessing colors' }
PharoDarkTheme >> classExtensionColor [
	^ Color gray lighter lighter
]

{ #category : #configure }
PharoDarkTheme >> configureMorphTreeMorph: aMorph [
	aMorph preferedPaneColor: self backgroundColor.
	aMorph 
		rowColorForEven: self backgroundColor
		odd: self backgroundColor.
]

{ #category : #configure }
PharoDarkTheme >> configurePluggableTextMorph: textMorph [ 
	textMorph 	
		color: self backgroundColor;
		textColor: self textColor;
		borderWidth: 1.
]

{ #category : #configure }
PharoDarkTheme >> configureScrollPane: aMorph [ 
	"aMorph color: self backgroundColor"
]

{ #category : #configure }
PharoDarkTheme >> configureSystemWindow: aWindow [
	
]

{ #category : #configure }
PharoDarkTheme >> configureTextMorph: textMorph [ 
	textMorph 
		"backgroundColor: self backgroundColor;"
		color: self textColor.
]

{ #category : #'border-styles' }
PharoDarkTheme >> configureWindowBorderFor: aWindow [
"	super configureWindowBorderFor: aWindow.
	aWindow roundedCorners: #()"
	| aStyle |
	
	aStyle := 
		SimpleBorder new 
			color: self borderColor;
			width: 1.
			
	aWindow borderStyle: aStyle.
]

{ #category : #'fill-styles-buttons' }
PharoDarkTheme >> controlButtonNormalFillStyleFor: aButton [
	"Return the normal control button fillStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."
	
	^self buttonNormalFillStyleFor: aButton
]

{ #category : #'accessing colors' }
PharoDarkTheme >> dangerTextColor [
	^ Color
		r: 239
		g: 83
		b: 80
		range: 255
]

{ #category : #'fill-styles' }
PharoDarkTheme >> desktopColor [

	^Color darkGray darker
]

{ #category : #'basic-colors-diffs' }
PharoDarkTheme >> diffAdditionalLineBackgroundColor [
	"Answer the color used to highlight additional text lines in a diff view."
	^ Color r: 0.0 g: 0.76 b: 0.0 alpha: 0.2
]

{ #category : #'basic-colors-diffs' }
PharoDarkTheme >> diffEdgeColor [
	"Answer the color used to show the border of the changes."
	^ Color white alpha: 0.5
]

{ #category : #'basic-colors-diffs' }
PharoDarkTheme >> diffModificatedLineBackgroundColor [
	"Answer the color used to highlight changed text lines in a diff view."
	^ Color gray alpha: 0.2
]

{ #category : #'basic-colors-diffs' }
PharoDarkTheme >> diffRemovedLinesBackgroundColor [
	"Answer the color used to highlight removed text lines in a diff view."
	^ Color r: 0.84 g: 0.0 b: 0.0 alpha: 0.2
]

{ #category : #'accessing colors' }
PharoDarkTheme >> disabledColor [ 
	^ Color gray lighter
]

{ #category : #'fill-styles' }
PharoDarkTheme >> dropListNormalFillStyleFor: aDropList [
	"Return the normal fillStyle for the given drop list."
	
	^ self backgroundColor
]

{ #category : #'border-styles' }
PharoDarkTheme >> dropListNormalListBorderStyleFor: aDropList [
	"Return the normal borderStyle for the list of the given given drop list"

	^BorderStyle inset
		width: 1;
		baseColor: self buttonColor lighter.
]

{ #category : #'fill-styles' }
PharoDarkTheme >> dropListNormalListFillStyleFor: aDropList [
	"Return the normal fillStyle for the list of the given drop list."
	
	^ self backgroundColor
]

{ #category : #'accessing colors' }
PharoDarkTheme >> enabledColor [ 
	^ self textColor
]

{ #category : #'basic-colors' }
PharoDarkTheme >> errorPopperBackgroundColor [
	^ Color red muchDarker 
]

{ #category : #private }
PharoDarkTheme >> glamorousBaseColorFor: aButton [
	^ self class baseColor
	"unfortunately, it looks like paneColor does not always return the wanted color"
	"aButton paneColorOrNil ifNil: [Color r: 200 g: 200 b: 200 range: 255]"
]

{ #category : #private }
PharoDarkTheme >> glamorousBasePassiveBackgroundColorFor: aButton [
	^ self class basePassiveBackgroundColor
]

{ #category : #private }
PharoDarkTheme >> glamorousBaseSelectionColorFor: aButton [
	^ self class baseSelectionColor
]

{ #category : #private }
PharoDarkTheme >> glamorousDarkBaseColorFor: aButton [
	^ self class darkBaseColor
]

{ #category : #private }
PharoDarkTheme >> glamorousLightColorFor: aButton [
	^ self class lightBaseColor
]

{ #category : #private }
PharoDarkTheme >> glamorousLightSelectionColorFor: aMorph [
	^ self class lightSelectionColor
]

{ #category : #private }
PharoDarkTheme >> glamorousNormalFillStyleFor: aMorph height: anInteger [
	"Return the normal button fillStyle for the given button."
	
"	| baseColor |
	baseColor := self glamorousBaseColorFor: aMorph.
	^ self glamorousNormalFillStyleWithBaseColor: baseColor for: aMorph height: anInteger
"

	^ SolidFillStyle color: (self glamorousLightColorFor: aMorph)
]

{ #category : #private }
PharoDarkTheme >> glamorousNormalFillStyleWithBaseColor: aColor for: aMorph height: anInteger [	
	| top bottom |
	top := aColor darker.
	bottom := aColor.
	^(GradientFillStyle ramp: {
			0.0->top.
			0.7->bottom.})
		origin: aMorph bounds origin;
		direction: 0 @ anInteger;
		radial: false
]

{ #category : #'initialize-release' }
PharoDarkTheme >> initialize [
	"self beCurrent"

	super initialize.
	self windowActiveDropShadowStyle: #diffuse.
	self settings preferGradientFill: false.
]

{ #category : #'accessing colors' }
PharoDarkTheme >> lessConspicuousColorFrom: aColor [

	^	aColor slightlyLighter
]

{ #category : #'accessing colors' }
PharoDarkTheme >> lightBackgroundColor [ 
	^ Color r: 0.218 g: 0.217 b: 0.217
]

{ #category : #'accessing colors' }
PharoDarkTheme >> lineNumberColor [ 
	^ Color gray lighter lighter
]

{ #category : #'fill-styles' }
PharoDarkTheme >> listNormalFillStyleFor: aList [
	"Return the normal fillStyle for the given list."
	
	^ self backgroundColor
]

{ #category : #'accessing colors' }
PharoDarkTheme >> listTextColor [
	"Answer the list text color to use."

	^ self textColor
]

{ #category : #'accessing colors' }
PharoDarkTheme >> menuBorderColor [
	^ self borderColor
]

{ #category : #'accessing colors' }
PharoDarkTheme >> menuBorderWidth [
	^ self borderWidth
]

{ #category : #defaults }
PharoDarkTheme >> menuItemDisabledTextColorFor: aMenuItem [

	^ Color lightGray
]

{ #category : #'initialize-release' }
PharoDarkTheme >> newRadioMarkerForm [
	"Answer a new checkbox marker form."

	^Form extent: 12@12 depth: 32
]

{ #category : #'accessing colors' }
PharoDarkTheme >> scrollbarColor [ 
	^ self buttonColor
]

{ #category : #'border-styles-scrollbars' }
PharoDarkTheme >> scrollbarNormalFillStyleFor: aScrollbar [
	"Return the normal scrollbar fillStyle for the given scrollbar."
	
	^ SolidFillStyle color: self backgroundColor lighter.
]

{ #category : #'border-styles-scrollbars' }
PharoDarkTheme >> scrollbarPressedThumbFillStyleFor: aScrollbar [
	"Return the normal scrollbar button fillStyle for the given scrollbar."
	
	^ self buttonSelectedFillStyleFor: aScrollbar
]

{ #category : #'accessing colors' }
PharoDarkTheme >> secondarySelectionColor [
	^ Color r: 0.31 g: 0.31 b: 0.36
]

{ #category : #'accessing colors' }
PharoDarkTheme >> selectionColor [
	^ Color r: 16r07 g: 16r63 b: 16rAD range: 255
]

{ #category : #'accessing colors' }
PharoDarkTheme >> selectionTextColor [
	^ Color r: 16rE0 g: 16rE2 b: 16rE4 range: 255
]

{ #category : #defaults }
PharoDarkTheme >> shStyleTable [

	^SHRBTextStyler darkStyleTable
]

{ #category : #'accessing colors' }
PharoDarkTheme >> spotlightWindowColor [ 
	^ self windowColor
]

{ #category : #'accessing colors' }
PharoDarkTheme >> successTextColor [
	^ Color green darker darker darker
]

{ #category : #'basic-colors' }
PharoDarkTheme >> taskbarItemLabelColorForCollapsed: aButton [
	"Answer the colour for the label of the given taskbar button, given that the corresponding window is collapsed."

	^ self textColor
]

{ #category : #'basic-colors' }
PharoDarkTheme >> taskbarItemLabelColorForExpanded: aButton [
	"Answer the colour for the label of the given taskbar button, given that the corresponding window is expanded."

	^ self textColor
]

{ #category : #'accessing colors' }
PharoDarkTheme >> textColor [
	^ Color white
]

{ #category : #'accessing colors' }
PharoDarkTheme >> textColorForNonStandardBackground [
	^ Color black
]

{ #category : #'fill-styles' }
PharoDarkTheme >> textEditorNormalFillStyleFor: aTextEditor [
	^ self backgroundColor
]

{ #category : #'accessing colors' }
PharoDarkTheme >> unfocusedSelectionColor [
	^ self secondarySelectionColor
]

{ #category : #'accessing colors' }
PharoDarkTheme >> warningTextColor [
	^ Color yellow darker darker
]

{ #category : #'fill-styles' }
PharoDarkTheme >> windowActiveLabelFillStyleFor: aWindow [
	^ self textColor
]

{ #category : #'label-styles' }
PharoDarkTheme >> windowLabelForText: aTextOrString [
	^ (super windowLabelForText: aTextOrString)
		color: self textColor;
		yourself.
]
