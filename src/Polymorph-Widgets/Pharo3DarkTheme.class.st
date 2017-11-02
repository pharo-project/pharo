"
A dark theme for pharo, inspired on 

http://blog.jetbrains.com/blog/2013/06/11/inside-darcula-look-and-feel-an-interview-with-konstantin-bulenkov/

This theme works well with the ""Dark"" text styling. 
You can have ""the full experience"", executing: 

self installFullTheme.
"
Class {
	#name : #Pharo3DarkTheme,
	#superclass : #UITheme,
	#category : 'Polymorph-Widgets-Themes'
}

{ #category : #accessing }
Pharo3DarkTheme class >> baseColor [
	^ Color veryDarkGray
]

{ #category : #accessing }
Pharo3DarkTheme class >> basePassiveBackgroundColor [
	^ Color darkGray
]

{ #category : #accessing }
Pharo3DarkTheme class >> baseSelectionColor [
	^ Color r: 97 g: 163 b: 225 range: 255
]

{ #category : #accessing }
Pharo3DarkTheme class >> darkBaseColor [
	^ Color veryDarkGray
]

{ #category : #accessing }
Pharo3DarkTheme class >> isAbstract [
	"Answer whether the receiver is considered to be abstract."

	^false
]

{ #category : #accessing }
Pharo3DarkTheme class >> lightBaseColor [
	^ Color darkGray
]

{ #category : #accessing }
Pharo3DarkTheme class >> lightSelectionColor [
	^ Color r: 175 g: 213 b: 250 range: 255
]

{ #category : #settings }
Pharo3DarkTheme class >> setPreferredPreferences [
	"NECPreferences 
		expandPrefixes: true;
		popupShowWithShortcut: Character tab asShortcut."
]

{ #category : #accessing }
Pharo3DarkTheme class >> themeName [
	^ 'Pharo3 Dark'
]

{ #category : #accessing }
Pharo3DarkTheme class >> veryLightSelectionColor [
	^ self lightSelectionColor muchLighter
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> backgroundColor [
	^ Color r: 0.130 g: 0.130 b: 0.130
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> balloonBackgroundColor [

	^Color veryDarkGray lighter
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> borderColor [ 
	^ Color darkGray darker darker darker
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> buttonColor [
	^ self backgroundColor lighter lighter
]

{ #category : #border-styles-buttons }
Pharo3DarkTheme >> buttonNormalBorderStyleFor: aButton [
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

{ #category : #fill-styles-buttons }
Pharo3DarkTheme >> buttonNormalFillStyleFor: aButton [
	"Return the normal button fillStyle for the given button."
	
	(aButton valueOfProperty: #noFill ifAbsent: [false]) 
		ifTrue: [ ^ SolidFillStyle color: Color transparent ].
		
	^ SolidFillStyle color: self buttonColor.
]

{ #category : #fill-styles-buttons }
Pharo3DarkTheme >> buttonSelectedFillStyleFor: aButton [
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
Pharo3DarkTheme >> caretColor [
	^ Color lightBlue.
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> classExtensionColor [
	^ Color gray lighter lighter
]

{ #category : #configure }
Pharo3DarkTheme >> configureMorphTreeMorph: aMorph [
	aMorph preferedPaneColor: self backgroundColor.
	aMorph 
		rowColorForEven: self backgroundColor
		odd: self backgroundColor.
]

{ #category : #configure }
Pharo3DarkTheme >> configurePluggableTextMorph: textMorph [ 
	textMorph 	
		color: self backgroundColor;
		textColor: self textColor;
		borderWidth: 1.
]

{ #category : #configure }
Pharo3DarkTheme >> configureScrollPane: aMorph [ 
	"aMorph color: self backgroundColor"
]

{ #category : #configure }
Pharo3DarkTheme >> configureSystemWindow: aWindow [
	
]

{ #category : #configure }
Pharo3DarkTheme >> configureTextMorph: textMorph [ 
	textMorph 
		"backgroundColor: self backgroundColor;"
		color: self textColor.
]

{ #category : #border-styles }
Pharo3DarkTheme >> configureWindowBorderFor: aWindow [
"	super configureWindowBorderFor: aWindow.
	aWindow roundedCorners: #()"
	| aStyle |
	
	aStyle := 
		SimpleBorder new 
			color: self borderColor;
			width: 1.
			
	aWindow borderStyle: aStyle.
]

{ #category : #fill-styles-buttons }
Pharo3DarkTheme >> controlButtonNormalFillStyleFor: aButton [
	"Return the normal control button fillStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."
	
	^self buttonNormalFillStyleFor: aButton
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> dangerTextColor [
	^ Color
		r: 239
		g: 83
		b: 80
		range: 255
]

{ #category : #fill-styles }
Pharo3DarkTheme >> desktopColor [

	^Color darkGray darker
]

{ #category : #basic-colors-diffs }
Pharo3DarkTheme >> diffAdditionalLineBackgroundColor [
	"Answer the color used to highlight additional text lines in a diff view."
	^ Color r: 0.0 g: 0.76 b: 0.0 alpha: 0.2
]

{ #category : #basic-colors-diffs }
Pharo3DarkTheme >> diffEdgeColor [
	"Answer the color used to show the border of the changes."
	^ Color white alpha: 0.5
]

{ #category : #basic-colors-diffs }
Pharo3DarkTheme >> diffModificatedLineBackgroundColor [
	"Answer the color used to highlight changed text lines in a diff view."
	^ Color gray alpha: 0.2
]

{ #category : #basic-colors-diffs }
Pharo3DarkTheme >> diffRemovedLinesBackgroundColor [
	"Answer the color used to highlight removed text lines in a diff view."
	^ Color r: 0.84 g: 0.0 b: 0.0 alpha: 0.2
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> disabledColor [ 
	^ Color gray lighter
]

{ #category : #fill-styles }
Pharo3DarkTheme >> dropListNormalFillStyleFor: aDropList [
	"Return the normal fillStyle for the given drop list."
	
	^ self backgroundColor
]

{ #category : #border-styles }
Pharo3DarkTheme >> dropListNormalListBorderStyleFor: aDropList [
	"Return the normal borderStyle for the list of the given given drop list"

	^BorderStyle inset
		width: 1;
		baseColor: self buttonColor lighter.
]

{ #category : #fill-styles }
Pharo3DarkTheme >> dropListNormalListFillStyleFor: aDropList [
	"Return the normal fillStyle for the list of the given drop list."
	
	^ self backgroundColor
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> enabledColor [ 
	^ self textColor
]

{ #category : #basic-colors }
Pharo3DarkTheme >> errorPopperBackgroundColor [
	^ Color red muchDarker 
]

{ #category : #private }
Pharo3DarkTheme >> glamorousBaseColorFor: aButton [
	^ self class baseColor
	"unfortunately, it looks like paneColor does not always return the wanted color"
	"aButton paneColorOrNil ifNil: [Color r: 200 g: 200 b: 200 range: 255]"
]

{ #category : #private }
Pharo3DarkTheme >> glamorousBasePassiveBackgroundColorFor: aButton [
	^ self class basePassiveBackgroundColor
]

{ #category : #private }
Pharo3DarkTheme >> glamorousBaseSelectionColorFor: aButton [
	^ self class baseSelectionColor
]

{ #category : #private }
Pharo3DarkTheme >> glamorousDarkBaseColorFor: aButton [
	^ self class darkBaseColor
]

{ #category : #private }
Pharo3DarkTheme >> glamorousLightColorFor: aButton [
	^ self class lightBaseColor
]

{ #category : #private }
Pharo3DarkTheme >> glamorousLightSelectionColorFor: aMorph [
	^ self class lightSelectionColor
]

{ #category : #private }
Pharo3DarkTheme >> glamorousNormalFillStyleFor: aMorph height: anInteger [
	"Return the normal button fillStyle for the given button."
	
"	| baseColor |
	baseColor := self glamorousBaseColorFor: aMorph.
	^ self glamorousNormalFillStyleWithBaseColor: baseColor for: aMorph height: anInteger
"

	^ SolidFillStyle color: (self glamorousLightColorFor: aMorph)
]

{ #category : #private }
Pharo3DarkTheme >> glamorousNormalFillStyleWithBaseColor: aColor for: aMorph height: anInteger [	
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

{ #category : #initialize-release }
Pharo3DarkTheme >> initialize [
	"self beCurrent"

	super initialize.
	self windowActiveDropShadowStyle: #diffuse.
	self settings preferGradientFill: false.
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> lessConspicuousColorFrom: aColor [

	^	aColor slightlyLighter
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> lightBackgroundColor [ 
	^ Color r: 0.218 g: 0.217 b: 0.217
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> lineNumberColor [ 
	^ Color gray lighter lighter
]

{ #category : #fill-styles }
Pharo3DarkTheme >> listNormalFillStyleFor: aList [
	"Return the normal fillStyle for the given list."
	
	^ self backgroundColor
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> listTextColor [
	"Answer the list text color to use."

	^ self textColor
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> menuBorderColor [
	^ self borderColor
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> menuBorderWidth [
	^ self borderWidth
]

{ #category : #defaults }
Pharo3DarkTheme >> menuItemDisabledTextColorFor: aMenuItem [

	^ Color lightGray
]

{ #category : #initialize-release }
Pharo3DarkTheme >> newRadioMarkerForm [
	"Answer a new checkbox marker form."

	^Form extent: 12@12 depth: 32
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> scrollbarColor [ 
	^ self buttonColor
]

{ #category : #border-styles-scrollbars }
Pharo3DarkTheme >> scrollbarNormalFillStyleFor: aScrollbar [
	"Return the normal scrollbar fillStyle for the given scrollbar."
	
	^ SolidFillStyle color: self backgroundColor lighter.
]

{ #category : #border-styles-scrollbars }
Pharo3DarkTheme >> scrollbarPressedThumbFillStyleFor: aScrollbar [
	"Return the normal scrollbar button fillStyle for the given scrollbar."
	
	^ self buttonSelectedFillStyleFor: aScrollbar
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> secondarySelectionColor [
	^ Color r: 0.31 g: 0.31 b: 0.36
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> selectionColor [
	^ Color r: 16r07 g: 16r63 b: 16rAD range: 255
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> selectionTextColor [
	^ Color r: 16rE0 g: 16rE2 b: 16rE4 range: 255
]

{ #category : #defaults }
Pharo3DarkTheme >> shStyleTable [

	^SHTextStylerST80 darkStyleTable
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> spotlightWindowColor [ 
	^ self windowColor
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> successTextColor [
	^ Color green darker darker darker
]

{ #category : #basic-colors }
Pharo3DarkTheme >> taskbarButtonLabelColorForCollapsed: aButton [
	"Answer the colour for the label of the given taskbar button, given that the corresponding window is collapsed."

	^ self textColor
]

{ #category : #basic-colors }
Pharo3DarkTheme >> taskbarButtonLabelColorForExpanded: aButton [
	"Answer the colour for the label of the given taskbar button, given that the corresponding window is expanded."

	^ self textColor
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> textColor [
	^ Color white
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> textColorForNonStandardBackground [
	^ Color black
]

{ #category : #fill-styles }
Pharo3DarkTheme >> textEditorNormalFillStyleFor: aTextEditor [
	^ self backgroundColor
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> unfocusedSelectionColor [
	^ self secondarySelectionColor
]

{ #category : #'accessing colors' }
Pharo3DarkTheme >> warningTextColor [
	^ Color yellow darker darker
]

{ #category : #fill-styles }
Pharo3DarkTheme >> windowActiveLabelFillStyleFor: aWindow [
	^ self textColor
]

{ #category : #label-styles }
Pharo3DarkTheme >> windowLabelForText: aTextOrString [
	^ (super windowLabelForText: aTextOrString)
		color: self textColor;
		yourself.
]
