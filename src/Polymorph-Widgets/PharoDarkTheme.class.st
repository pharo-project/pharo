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

{ #category : #configuration }
PharoDarkTheme class >> colorConfiguratorClass [

	^ DarkThemeColorConfigurator
]

{ #category : #accessing }
PharoDarkTheme class >> isAbstract [
	"Answer whether the receiver is considered to be abstract."

	^false
]

{ #category : #accessing }
PharoDarkTheme class >> themeName [
	^ 'Pharo Dark'
]

{ #category : #'border-styles - buttons' }
PharoDarkTheme >> buttonDisabledBorderStyleFor: aButton [

	"Return the disabled button fillStyle for the given color."

	^ BorderStyle width: 0 color: Color transparent
]

{ #category : #'fill-styles - buttons' }
PharoDarkTheme >> buttonDisabledFillStyleFor: aButton [

	"Return the disabled button fillStyle for the given color."

	^ SolidFillStyle color: Color transparent
]

{ #category : #'border-styles - buttons' }
PharoDarkTheme >> buttonNormalBorderStyleFor: aButton [

	"Return the normal button borderStyle for the given button."

	(aButton valueOfProperty: #noBorder ifAbsent: [ false ]) ifTrue: [ 
		^ SimpleBorderStyle new
			  width: 0;
			  baseColor: Color transparent ].
	^ SimpleBorderStyle new
		  width: 1;
		  baseColor: self buttonColor lighter
]

{ #category : #'fill-styles - buttons' }
PharoDarkTheme >> buttonNormalFillStyleFor: aButton [

	"Return the normal button fillStyle for the given button."

	(aButton valueOfProperty: #noFill ifAbsent: [ false ]) ifTrue: [ 
		^ SolidFillStyle color: Color transparent ].
	^ SolidFillStyle color: self buttonColor
]

{ #category : #'fill-styles - buttons' }
PharoDarkTheme >> buttonSelectedFillStyleFor: aButton [

	"Return the normal button fillStyle for the given button."

	^ SolidFillStyle color: self selectionColor
]

{ #category : #configure }
PharoDarkTheme >> configureMorphTreeMorph: aMorph [

	aMorph preferedPaneColor: self backgroundColor.
	aMorph rowColorForEven: self backgroundColor odd: self backgroundColor
]

{ #category : #configure }
PharoDarkTheme >> configurePluggableTextMorph: textMorph [

	textMorph
		color: self backgroundColor;
		textColor: self textColor;
		borderWidth: 1
]

{ #category : #configure }
PharoDarkTheme >> configureScrollPane: aMorph [

	
]

{ #category : #configure }
PharoDarkTheme >> configureSystemWindow: aWindow [

	
]

{ #category : #configure }
PharoDarkTheme >> configureTextMorph: textMorph [

	textMorph color: self textColor
]

{ #category : #'border-styles' }
PharoDarkTheme >> configureWindowBorderFor: aWindow [

	| aStyle |
	aStyle := SimpleBorderStyle new
		          color: self borderColor;
		          width: 1.
	aWindow borderStyle: aStyle
]

{ #category : #'fill-styles - buttons' }
PharoDarkTheme >> controlButtonNormalFillStyleFor: aButton [

	"Return the normal control button fillStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^ self buttonNormalFillStyleFor: aButton
]

{ #category : #'border-styles' }
PharoDarkTheme >> dropListNormalListBorderStyleFor: aDropList [

	"Return the normal borderStyle for the list of the given given drop list"

	^ BorderStyle inset
		  width: 1;
		  baseColor: self buttonColor lighter
]

{ #category : #initialization }
PharoDarkTheme >> initialize [

	super initialize.
	self windowActiveDropShadowStyle: #diffuse.
	self settings preferGradientFill: false.
]

{ #category : #'label-styles' }
PharoDarkTheme >> labelDisabledColorFor: aLabel [

	"Answer the color to use for a label when disabled."

	^ aLabel owner ifNil: [ aLabel paneColor muchDarker ] ifNotNil: [ 
		  aLabel owner color isTransparent ifTrue: [ aLabel owner paneColor muchLighter ]
			  ifFalse: [ aLabel owner color muchLighter ] ]
]

{ #category : #accessing }
PharoDarkTheme >> menuBorderWidth [

	^ self borderWidth * self displayScaleFactor
]

{ #category : #accessing }
PharoDarkTheme >> newRadioMarkerForm [

	"Answer a new checkbox marker form."

	^ Form extent: 12 @ 12 depth: 32
]

{ #category : #'fill-styles - scrollbars' }
PharoDarkTheme >> scrollbarPressedThumbFillStyleFor: aScrollbar [

	"Return the normal scrollbar button fillStyle for the given scrollbar."

	^ self buttonSelectedFillStyleFor: aScrollbar
]

{ #category : #accessing }
PharoDarkTheme >> shStyleTableName [

	^ #darkStyleTable
]

{ #category : #'fill-styles - taskbar' }
PharoDarkTheme >> taskbarFillStyleFor: aTaskbar [

	^ SolidFillStyle color: (Color black lighter alpha: 0.8)
]

{ #category : #'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemDisabledBorderStyleFor: aButton [

	^ self taskbarItemNormalBorderStyleFor: aButton
]

{ #category : #'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemDisabledFillStyleFor: aButton [

	"Return the disabled taskbar item fillStyle for the given color."

	^ self taskbarItemNormalFillStyleFor: aButton
]

{ #category : #'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemMouseOverBorderStyleFor: aButton [

	^ BottomBorderStyle new
		  color: Color white;
		  width: 2;
		  yourself
]

{ #category : #'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemMouseOverFillStyleFor: aButton [

	^ SolidFillStyle color: (Color white alpha: 0.15)
]

{ #category : #'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemNormalBorderStyleFor: aButton [

	^ BottomBorderStyle new
		  color: Color white;
		  width: 1;
		  trimSize: 2;
		  yourself
]

{ #category : #'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemNormalFillStyleFor: aButton [

	^ SolidFillStyle color: Color transparent
]

{ #category : #'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemPressedBorderStyleFor: aButton [

	^ self taskbarItemNormalBorderStyleFor: aButton
]

{ #category : #'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemPressedFillStyleFor: aButton [

	^ SolidFillStyle color: (Color white alpha: 0.4)
]

{ #category : #'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedBorderStyleFor: aButton [

	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : #'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedDisabledBorderStyleFor: aButton [

	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : #'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedDisaledFillStyleFor: aButton [

	^ self taskbarItemSelectedFillStyleFor: aButton
]

{ #category : #'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedFillStyleFor: aButton [

	^ SolidFillStyle color: (Color white alpha: 0.15)
]

{ #category : #'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedMouseOverBorderStyleFor: aButton [

	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : #'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedMouseOverFillStyleFor: aButton [

	^ SolidFillStyle color: (Color white alpha: 0.25)
]

{ #category : #'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedPressedBorderStyleFor: aButton [

	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : #'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedPressedFillStyleFor: aButton [

	^ SolidFillStyle color: (Color white alpha: 0.35)
]

{ #category : #'label-styles - windows' }
PharoDarkTheme >> windowLabelForText: aTextOrString [

	^ (super windowLabelForText: aTextOrString)
		  color: self textColor;
		  yourself
]
