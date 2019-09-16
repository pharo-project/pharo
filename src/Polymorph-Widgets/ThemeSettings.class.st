"
Configurable basic settings for themes.
"
Class {
	#name : #ThemeSettings,
	#superclass : #Model,
	#instVars : [
		'windowColor',
		'selectionColor',
		'autoSelectionColor',
		'buttonColor',
		'scrollbarColor',
		'standardColorsOnly',
		'progressBarColor',
		'progressBarProgressColor',
		'menuColor',
		'menuTitleColor',
		'preferGradientFill',
		'preferRoundCorner',
		'autoMenuColor',
		'flatMenu',
		'fadedBackgroundWindows',
		'fastDragging',
		'animationSettings',
		'scrollBarThickness',
		'balloonHelpEnabled',
		'tabAmongFields',
		'balloonHelpDelayTime',
		'maxBalloonHelpLineLength',
		'desktopImageLayout',
		'menuSelectionColor',
		'findReplaceSelectionColor',
		'selectionBarColor',
		'unfocusedSelectionColor',
		'menuSelectionTextColor',
		'selectionTextColor',
		'secondarySelectionTextColor',
		'findReplaceSelectionTextColor'
	],
	#category : #'Polymorph-Widgets-Themes'
}

{ #category : #accessing }
ThemeSettings >> animationSettings [
	^ animationSettings ifNil: [animationSettings := AnimationSettings new]
]

{ #category : #settings }
ThemeSettings >> applySettingsFrom: aThemeSettings [
	"Change the settings to be those of aThemeSettings."
	
	self
		windowColor: aThemeSettings windowColor;
		selectionColor: aThemeSettings selectionColor;
		autoSelectionColor: aThemeSettings autoSelectionColor
]

{ #category : #menu }
ThemeSettings >> autoMenuColor [
	"Answer the value of autoMenuColor"

	^autoMenuColor ifNil: [^false]
]

{ #category : #menu }
ThemeSettings >> autoMenuColor: aBoolean [
	"Answer the value of autoMenuColor"

	autoMenuColor := aBoolean
]

{ #category : #accessing }
ThemeSettings >> autoSelectionColor [
	"Answer the value of autoSelectionColor"

	^autoSelectionColor ifNil: [^true]
]

{ #category : #accessing }
ThemeSettings >> autoSelectionColor: anObject [
	"Set the value of autoSelectionColor"

	autoSelectionColor := anObject.
	self
		changed: #autoSelectionColor;
		changed: #manualSelectionColor;
		changed: #selectionColor
]

{ #category : #balloon }
ThemeSettings >> balloonHelpDelayTime [
	^ balloonHelpDelayTime ifNil: [balloonHelpDelayTime := 800]
]

{ #category : #balloon }
ThemeSettings >> balloonHelpDelayTime: anInteger [
	balloonHelpDelayTime := anInteger
]

{ #category : #balloon }
ThemeSettings >> balloonHelpEnabled [
	^ balloonHelpEnabled ifNil: [balloonHelpEnabled := true]
]

{ #category : #balloon }
ThemeSettings >> balloonHelpEnabled: aBoolean [
	balloonHelpEnabled := aBoolean
]

{ #category : #accessing }
ThemeSettings >> basicSelectionColor [
	"Answer the recorded selection color rather than derived."

	^selectionColor
]

{ #category : #button }
ThemeSettings >> buttonColor [
	"Answer the value of buttonColor"

	^buttonColor ifNil: [self windowColor]
]

{ #category : #button }
ThemeSettings >> buttonColor: anObject [
	"Set the value of buttonColor"

	buttonColor := anObject
]

{ #category : #button }
ThemeSettings >> defaultButtonColor [
	"Answer the default button colour."

	^Color gray
]

{ #category : #accessing }
ThemeSettings >> defaultScrollbarColor [
	"Answer the default scrollbar colour."

	^Color gray
]

{ #category : #accessing }
ThemeSettings >> defaultSelectionColor [
	"Answer the default selection colour."

	^self defaultWindowColor
]

{ #category : #accessing }
ThemeSettings >> defaultWindowColor [
	"Answer the default window colour."

	^ Color lightGray
]

{ #category : #menu }
ThemeSettings >> derivedMenuColor [
	"Answer a selection colour based on the window color."

	| autoColor worldColor |
	autoColor := self autoMenuColor 
				and: [Display depth > 4
						and: [(worldColor := self currentWorld color) isColor]].

	^ autoColor
				ifTrue: [worldColor luminance > 0.7
						ifTrue: [worldColor mixed: 0.85 with: Color black]
						ifFalse: [worldColor mixed: 0.4 with: Color white]]
				ifFalse: [self menuColor].

]

{ #category : #accessing }
ThemeSettings >> derivedSelectionColor [
	"Answer a selection colour based on the window color."

	^self windowColor whiter lighter
]

{ #category : #accessing }
ThemeSettings >> desktopImageLayout [
	"Answer the layout symbol for any desktop image.
	See AlphaImageMorph>>layoutSymbols."

	^desktopImageLayout ifNil: [desktopImageLayout := #scaled]
]

{ #category : #accessing }
ThemeSettings >> desktopImageLayout: aSymbol [
	"Set the layout symbol for any desktop image.
	See AlphaImageMorph>>layoutSymbols."

	desktopImageLayout := aSymbol
]

{ #category : #accessing }
ThemeSettings >> fadedBackgroundWindows [
	^ fadedBackgroundWindows ifNil: [fadedBackgroundWindows := true]
]

{ #category : #accessing }
ThemeSettings >> fadedBackgroundWindows: aBoolean [
	fadedBackgroundWindows := aBoolean
]

{ #category : #menu }
ThemeSettings >> fallbackMenuColor [
	"Answer the value of menuColor"

	^ self preferGradientFill ifTrue: [Color lightGray] ifFalse: [Color lightGray muchLighter]
]

{ #category : #accessing }
ThemeSettings >> fastDragging [ 
		^ fastDragging ifNil: [fastDragging := false] 
]

{ #category : #accessing }
ThemeSettings >> fastDragging: aBoolean [
		fastDragging := aBoolean
]

{ #category : #accessing }
ThemeSettings >> findReplaceSelectionColor [
	"Answer the value of the selectionColor for find/replace"

	^ findReplaceSelectionColor ifNil: [findReplaceSelectionColor := Color orange alpha: 0.4]
]

{ #category : #accessing }
ThemeSettings >> findReplaceSelectionColor: anColor [
	findReplaceSelectionColor := anColor
]

{ #category : #accessing }
ThemeSettings >> findReplaceSelectionTextColor [
	"Answer the value of the selectionTextColor for find/replace"

	^ findReplaceSelectionTextColor 
		ifNil: [ self findReplaceSelectionColor contrastingBlackAndWhiteColor ]
]

{ #category : #accessing }
ThemeSettings >> findReplaceSelectionTextColor: aColor [
	"Answer the value of the selectionTextColor for find/replace"

	findReplaceSelectionTextColor := aColor
]

{ #category : #menu }
ThemeSettings >> flatMenu [ 
		^ flatMenu ifNil: [flatMenu := false]
]

{ #category : #menu }
ThemeSettings >> flatMenu: aBoolean [
		flatMenu := aBoolean
]

{ #category : #settings }
ThemeSettings >> fromConfiguration: tree [
	"Update the settings from the given tree."
	
	(tree at: #windowColor ifAbsent: []) ifNotNil: [:v | self windowColor: v].
	(tree at: #autoSelectionColor ifAbsent: []) ifNotNil: [:v | self autoSelectionColor: v].
	(tree at: #selectionColor ifAbsent: []) ifNotNil: [:v | self selectionColor: v]
]

{ #category : #accessing }
ThemeSettings >> haveFindReplaceSelectionTextColor [

	^ findReplaceSelectionTextColor notNil
]

{ #category : #accessing }
ThemeSettings >> haveSecondarySelectionTextColor [
	"Answer the value of selectionTextColor"

	^ secondarySelectionTextColor notNil
]

{ #category : #accessing }
ThemeSettings >> haveSelectionTextColor [
	
	^ selectionTextColor notNil
]

{ #category : #initialization }
ThemeSettings >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		autoSelectionColor: true;
		windowColor: self defaultWindowColor;
		selectionColor: self defaultSelectionColor;
		buttonColor: self defaultButtonColor;
		scrollbarColor: self defaultScrollbarColor;
		standardColorsOnly: false
]

{ #category : #accessing }
ThemeSettings >> manualSelectionColor [
	"Answer the inverse of autoSelectionColor."

	^self autoSelectionColor not
]

{ #category : #balloon }
ThemeSettings >> maxBalloonHelpLineLength [
	^ maxBalloonHelpLineLength ifNil: [maxBalloonHelpLineLength := 40]
]

{ #category : #balloon }
ThemeSettings >> maxBalloonHelpLineLength: anInteger [
	maxBalloonHelpLineLength := anInteger
]

{ #category : #menu }
ThemeSettings >> menuBorderColor [
	"Answer the value of menuColor"

	^ self flatMenu  
		ifFalse: [#raised]
		ifTrue: [self derivedMenuColor twiceDarker]
]

{ #category : #menu }
ThemeSettings >> menuBorderWidth [
	"Answer the value of menuColor"

	^ 2 * self currentWorld displayScaleFactor
]

{ #category : #menu }
ThemeSettings >> menuColor [
	"Answer the value of menuColor"

	^ menuColor ifNil: [self fallbackMenuColor]
]

{ #category : #menu }
ThemeSettings >> menuColor: anObject [
	"Set the value of menuColor"

	menuColor := anObject
]

{ #category : #menu }
ThemeSettings >> menuKeyboardControl [
	^ true
]

{ #category : #menu }
ThemeSettings >> menuKeyboardFocusColor [
	^ (Color r: 0.6 g: 1.0 b: 1.0)
]

{ #category : #menu }
ThemeSettings >> menuSelectionColor [
	"Answer the value of menuSelectionColor"

	^menuSelectionColor ifNil: [self derivedMenuColor darker]
]

{ #category : #menu }
ThemeSettings >> menuSelectionColor: aColor [
	"Set the base value of menuSelectionColor.
	If nil, actual color used will be based on derived menu color."

	menuSelectionColor := aColor
]

{ #category : #menu }
ThemeSettings >> menuSelectionTextColor [
	"Answer the value of menuSelectionTextColor"

	^menuSelectionTextColor ifNil: [menuSelectionTextColor := Color black]
]

{ #category : #menu }
ThemeSettings >> menuSelectionTextColor: aColor [
	"Answer the value of menuSelectionTextColor"

	menuSelectionTextColor := aColor
]

{ #category : #menu }
ThemeSettings >> menuShadowColor [
	^ Color r: 0.0 g: 0.0 b: 0.0 alpha: 0.666
]

{ #category : #menu }
ThemeSettings >> menuTitleBorderColor [
	^ self flatMenu
		ifFalse: [#inset]
		ifTrue: [self derivedMenuColor muchDarker]
]

{ #category : #menu }
ThemeSettings >> menuTitleColor [
	"Answer the value of menuTitleColor"

	Display depth = 1 ifTrue: [^ Color white].
	Display depth = 2 ifTrue: [^ Color gray].
	^menuTitleColor 
		ifNil: [self menuColor 
				ifNil: [Color gray] 
				ifNotNil: [:mc | self preferGradientFill 
					ifTrue: [mc]
					ifFalse: [mc twiceDarker]]]
]

{ #category : #menu }
ThemeSettings >> menuTitleColor: anObject [
	"Set the value of menuTitleColor"

	menuTitleColor := anObject
]

{ #category : #accessing }
ThemeSettings >> preferGradientFill [ 
	"Answer the value of preferGradientFill "

	^preferGradientFill  ifNil: [preferGradientFill := self preferGradientFillByDefault]
]

{ #category : #accessing }
ThemeSettings >> preferGradientFill: aBoolean [
	"Set the value of preferGradientFill "

	preferGradientFill  := aBoolean
]

{ #category : #accessing }
ThemeSettings >> preferGradientFillByDefault [
	^ true
]

{ #category : #accessing }
ThemeSettings >> preferRoundCorner [
	"Answer the value of preferRoundCorner"

	^preferRoundCorner  ifNil: [preferRoundCorner := self preferRoundCornerByDefault]
]

{ #category : #accessing }
ThemeSettings >> preferRoundCorner: aBoolean [
	"Set the value of preferRoundCorner"

	preferRoundCorner := aBoolean
]

{ #category : #accessing }
ThemeSettings >> preferRoundCornerByDefault [
	^ true
]

{ #category : #progressbar }
ThemeSettings >> progressBarColor [
	"Answer the value of progressBarColor"

	^progressBarColor ifNil: [progressBarColor := self derivedMenuColor muchLighter]
]

{ #category : #progressbar }
ThemeSettings >> progressBarColor: anObject [
	"Set the value of progressBarColor"

	progressBarColor := anObject
]

{ #category : #progressbar }
ThemeSettings >> progressBarProgressColor [
	"Answer the value of progressBarProgressColor"

	^progressBarProgressColor ifNil: [self menuTitleColor]
]

{ #category : #progressbar }
ThemeSettings >> progressBarProgressColor: anObject [
	"Set the value of progressBarProgressColor"

	progressBarProgressColor := anObject
]

{ #category : #accessing }
ThemeSettings >> scrollBarThickness [
	^ scrollBarThickness ifNil: [scrollBarThickness := 14]
]

{ #category : #accessing }
ThemeSettings >> scrollbarColor [
	"Answer the value of scrollbarColor"

	^scrollbarColor ifNil: [self windowColor]
]

{ #category : #accessing }
ThemeSettings >> scrollbarColor: anObject [
	"Set the value of scrollbarColor"

	scrollbarColor := anObject
]

{ #category : #accessing }
ThemeSettings >> secondarySelectionTextColor [
	"Answer the value of selectionTextColor"

	^ secondarySelectionTextColor 
		ifNil: [self theme secondarySelectionColor contrastingBlackAndWhiteColor ]
]

{ #category : #accessing }
ThemeSettings >> secondarySelectionTextColor: aColor [
	"set the value of selectionTextColor"

	secondarySelectionTextColor := aColor
]

{ #category : #accessing }
ThemeSettings >> selectionBarColor [
	^ selectionBarColor ifNil: [selectionBarColor := self selectionColor alpha: 0.06]
]

{ #category : #accessing }
ThemeSettings >> selectionBarColor: aColor [
	selectionBarColor := aColor
]

{ #category : #accessing }
ThemeSettings >> selectionColor [
	"Answer the value of selectionColor"
	
	Display depth = 1 ifTrue: [^ Color veryLightGray].
	Display depth = 2 ifTrue: [^ Color gray].
	^self autoSelectionColor
		ifTrue: [self derivedSelectionColor]
		ifFalse: [selectionColor ifNil: [selectionColor := Color r: 0.0 g: 0.0 b: 0.8 alpha: 0.25]]
]

{ #category : #accessing }
ThemeSettings >> selectionColor: anObject [
	"Set the value of selectionColor"
	selectionColor := anObject
]

{ #category : #accessing }
ThemeSettings >> selectionTextColor [
	"Answer the value of selectionTextColor"
	
	^selectionTextColor ifNil: [Color black ]
]

{ #category : #accessing }
ThemeSettings >> selectionTextColor: aColor [
	selectionTextColor := aColor
]

{ #category : #accessing }
ThemeSettings >> standardColorsOnly [
	"Answer the value of standardColorsOnly"
 
	^standardColorsOnly ifNil: [^false]
]

{ #category : #accessing }
ThemeSettings >> standardColorsOnly: anObject [
	"Set the value of standardColorsOnly"

	standardColorsOnly := anObject
]

{ #category : #accessing }
ThemeSettings >> tabAmongFields [
	^ tabAmongFields ifNil: [tabAmongFields := true]
]

{ #category : #accessing }
ThemeSettings >> tabAmongFields: aBoolean [
	tabAmongFields := aBoolean
]

{ #category : #settings }
ThemeSettings >> toConfiguration: tree [
	"Update the tree from the receiver."
	
	tree at: #windowColor put: self windowColor.
	tree at: #autoSelectionColor put: self autoSelectionColor.
	tree at: #selectionColor put: self basicSelectionColor
]

{ #category : #accessing }
ThemeSettings >> unfocusedSelectionColor [
	"Answer the value of selectionColor when the window have not the focus"

	^ unfocusedSelectionColor ifNil: [unfocusedSelectionColor := self selectionColor alphaMixed: 0.7 with: Color veryLightGray lighter]
]

{ #category : #accessing }
ThemeSettings >> unfocusedSelectionColor: aColor [
	"Answer the value of selectionColor when the window have not the focus"

	unfocusedSelectionColor := aColor
]

{ #category : #accessing }
ThemeSettings >> windowColor [
	"Answer the value of windowColor"

	^ windowColor ifNil: [Color lightGray]
]

{ #category : #accessing }
ThemeSettings >> windowColor: anObject [
	"Set the value of windowColor"

	windowColor := anObject.
	self changed: #windowColor.
	self autoSelectionColor ifTrue: [self changed: #selectionColor]
]
