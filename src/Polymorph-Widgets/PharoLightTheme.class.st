"
The theme is developed in the context of the Glamour project, and its goal is to create a look that:
- does not look like a specific operating system. In particular, the icons should be operating system agnostic, because, for example, people in Windows are confused by the red, yellow, green buttons of apple.
- uses a limited amount of colors and effects.
- is fast.

self defaultSettings: nil.
self beCurrent. 

Is adopted as the theme for Pharo3, but it was renamed to avoid collisions with the glamour team,
"
Class {
	#name : #PharoLightTheme,
	#superclass : #UITheme,
	#category : #'Polymorph-Widgets-Themes'
}

{ #category : #accessing }
PharoLightTheme class >> baseColor [
	^ Color r: 210 g: 210 b: 210 range: 255
]

{ #category : #accessing }
PharoLightTheme class >> basePassiveBackgroundColor [
	^ Color r: 245 g: 245 b: 245 range: 255
]

{ #category : #accessing }
PharoLightTheme class >> baseSelectionColor [
	^ Color r: 97 g: 163 b: 225 range: 255
]

{ #category : #accessing }
PharoLightTheme class >> darkBaseColor [
	^ Color r: 200 g: 200 b: 200 range: 255
]

{ #category : #accessing }
PharoLightTheme class >> isAbstract [
	"Answer whether the receiver is considered to be abstract."

	^false
]

{ #category : #accessing }
PharoLightTheme class >> lightBaseColor [
	^ Color r: 230 g: 230 b: 230 range: 255
]

{ #category : #accessing }
PharoLightTheme class >> lightSelectionColor [
	^ Color r: 175 g: 213 b: 250 range: 255
]

{ #category : #settings }
PharoLightTheme class >> setPreferredPreferences [
	"NECPreferences 
		expandPrefixes: true;
		popupShowWithShortcut: Character tab asShortcut."
]

{ #category : #settings }
PharoLightTheme class >> setPreferredShoutColors [
	"self setPreferredShoutColors"

	SHRBTextStyler styleTable: 
		#(
			"(symbol color [emphasisSymbolOrArray [textStyleName [pixelHeight]]])" 
			(default 								black)
			(invalid 									red)
			(excessCode 							red)
			(comment 								(gray darker))
			(unfinishedComment 					(red muchDarker))
			(#'$' 									(red muchDarker))
			(character 								(red muchDarker))
			(integer 								(red muchDarker))
			(number 								(red muchDarker)) 
			(#- 										(red muchDarker))
			(symbol 								(magenta muchDarker)) 
			(stringSymbol 							(magenta muchDarker)) 
			(literalArray 							(magenta muchDarker))
			(string 									(magenta muchDarker) normal)
			(unfinishedString 						red normal)
			(assignment 							nil)
			(ansiAssignment 						nil)
			(literal 									nil italic)
			(keyword 								(black))
			(binary 									(black)) 
			(unary 									(black))
			(incompleteKeyword 					red)
			(incompleteBinary 						red) 
			(incompleteUnary 						red )
			(undefinedKeyword 					red)
			(undefinedBinary 						red) 
			(undefinedUnary 						red) 
			(patternKeyword 						nil bold)
			(patternBinary 							nil bold)
			(patternUnary 							nil bold) 
			(#self 									(cyan muchDarker ))
			(#super 								(cyan muchDarker ))
			(#true 									(red muchDarker))
			(#false 									(red muchDarker))
			(#nil 									(red muchDarker))
			(#thisContext 							(cyan muchDarker ))
			(#return 								(cyan muchDarker ) bold)
			(patternArg 							(blue muchDarker)) 
			(methodArg 							(blue muchDarker))
			(blockPatternArg 						(blue muchDarker))
			(blockArg 								(blue muchDarker))
			(argument 								(blue muchDarker))
			(blockArgColon 						black)
			(leftParenthesis 						black)
			(rightParenthesis 						black)
			(leftParenthesis1 						(green muchDarker))
			(rightParenthesis1 						(green muchDarker))
			(leftParenthesis2 						(magenta muchDarker))
			(rightParenthesis2 						(magenta muchDarker))
			(leftParenthesis3 						(red muchDarker))
			(rightParenthesis3 						(red muchDarker))
			(leftParenthesis4 						(green darker))
			(rightParenthesis4 						(green darker))
			(leftParenthesis5 						(orange darker))
			(rightParenthesis5 						(orange darker))
			(leftParenthesis6 						(magenta darker))
			(rightParenthesis6 						(magenta darker))
			(leftParenthesis7 						blue)
			(rightParenthesis7 						blue)
			(blockStart 								black)
			(blockEnd 								black)
			(blockStart1 							(green muchDarker))
			(blockEnd1 								(green muchDarker))
			(blockStart2 							(magenta muchDarker))
			(blockEnd2 								(magenta muchDarker))
			(blockStart3 							(red muchDarker))
			(blockEnd3 								(red muchDarker))
			(blockStart4 							(green darker))
			(blockEnd4 								(green darker))
			(blockStart5 							(orange darker))
			(blockEnd5 								(orange darker))
			(blockStart6 							(magenta darker))
			(blockEnd6 								(magenta darker))
			(blockStart7 							blue)
			(blockEnd7 								blue) 
			(arrayStart 								black)
			(arrayEnd 								black)
			(arrayStart1 							black)
			(arrayEnd1 								black)
			(leftBrace 								black)
			(rightBrace 								black)
			(cascadeSeparator 					black)
			(statementSeparator 					black)
			(externalCallType 						black)
			(externalCallTypePointerIndicator 		black)
			(primitiveOrExternalCallStart 			black bold)
			(primitiveOrExternalCallEnd 			black bold)
			(methodTempBar 						(black))
			(blockTempBar 						(black))
			(blockArgsBar 							(black))
			(primitive 								(green muchDarker))
			(pragmaKeyword 						(green muchDarker))
			(pragmaUnary 							(green muchDarker))
			(pragmaBinary 							(green muchDarker)) 
			(externalFunctionCallingConvention 	(green muchDarker) bold)
			(module 								(green muchDarker) bold)
			(blockTempVar 						(blue muchDarker))
			(blockPatternTempVar 					(blue muchDarker))
			(instVar 								(blue muchDarker))
			(workspaceVar 							(blue muchDarker))
			(undefinedIdentifier 					red)
			(incompleteIdentifier 					red)
			(tempVar 								(blue muchDarker))
			(patternTempVar 						(blue muchDarker))
			(poolConstant 							(blue muchDarker))
			(classVar 								(blue muchDarker))
			(globalVar 								(blue muchDarker)))

]

{ #category : #accessing }
PharoLightTheme class >> themeName [
	^ 'Pharo Light'
]

{ #category : #accessing }
PharoLightTheme class >> veryLightSelectionColor [
	^ self lightSelectionColor muchLighter
]

{ #category : #'accessing colors' }
PharoLightTheme >> balloonBackgroundColor [

	^ (Color r: 1.0 g: 1.0 b: 0.71 alpha: 1.0)
]

{ #category : #'border-styles-buttons' }
PharoLightTheme >> buttonNormalBorderStyleFor: aButton [
	"Return the normal button borderStyle for the given button."

	| outerColor |
	(aButton valueOfProperty: #noBorder ifAbsent: [false]) ifTrue: [
		^ SimpleBorder new width: 0; baseColor: Color transparent ].
	outerColor := self glamorousDarkBaseColorFor: aButton.
	^SimpleBorder new
					width: 1;
					baseColor: outerColor
]

{ #category : #'border-styles' }
PharoLightTheme >> configureWindowBorderFor: aWindow [
"	super configureWindowBorderFor: aWindow.
	aWindow roundedCorners: #()"
	| aStyle |
	
	aStyle := 
		SimpleBorder new 
			color: (Color lightGray);
			width: 1.
			
	aWindow borderStyle: aStyle.
]

{ #category : #private }
PharoLightTheme >> glamorousBaseColorFor: aButton [
	^ self class baseColor
	"unfortunately, it looks like paneColor does not always return the wanted color"
	"aButton paneColorOrNil ifNil: [Color r: 200 g: 200 b: 200 range: 255]"
]

{ #category : #private }
PharoLightTheme >> glamorousBasePassiveBackgroundColorFor: aButton [
	^ self class basePassiveBackgroundColor
]

{ #category : #private }
PharoLightTheme >> glamorousBaseSelectionColorFor: aButton [
	^ self class baseSelectionColor
]

{ #category : #private }
PharoLightTheme >> glamorousDarkBaseColorFor: aButton [
	^ self class darkBaseColor
]

{ #category : #private }
PharoLightTheme >> glamorousLightColorFor: aButton [
	^ self class lightBaseColor
]

{ #category : #private }
PharoLightTheme >> glamorousLightSelectionColorFor: aMorph [
	^ self class lightSelectionColor
]

{ #category : #private }
PharoLightTheme >> glamorousNormalFillStyleFor: aMorph height: anInteger [
	"Return the normal button fillStyle for the given button."
	
"	| baseColor |
	baseColor := self glamorousBaseColorFor: aMorph.
	^ self glamorousNormalFillStyleWithBaseColor: baseColor for: aMorph height: anInteger
"

	^ SolidFillStyle color: (self glamorousLightColorFor: aMorph)
]

{ #category : #private }
PharoLightTheme >> glamorousNormalFillStyleWithBaseColor: aColor for: aMorph height: anInteger [	
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

{ #category : #private }
PharoLightTheme >> glamorousSelectedFillStyleFor: aMorph height: anInteger [
	

	^ SolidFillStyle color: (self glamorousLightSelectionColorFor:  aMorph)

]

{ #category : #private }
PharoLightTheme >> glamorousVeryLightFillStyleFor: aMorph height: anInteger [
	"Return the very light button fillStyle for the given button."
	
"	| baseColor |
	baseColor := self glamorousBaseColorFor: aMorph.
	^ self glamorousNormalFillStyleWithBaseColor: baseColor for: aMorph height: anInteger
"
	^ SolidFillStyle color: (self glamorousVeyLightSelectionColorFor: aMorph)
]

{ #category : #private }
PharoLightTheme >> glamorousVeyLightSelectionColorFor: aMorph [
	^ self class veryLightSelectionColor
]

{ #category : #'initialize-release' }
PharoLightTheme >> newRadioMarkerForm [
	"Answer a new checkbox marker form."

	^Form extent: 12@12 depth: 32
]

{ #category : #forms }
PharoLightTheme >> scrollbarPressedThumbFillStyleFor: aScrollbar [
	"Return the pressed scrollbar thumb fillStyle for the given scrollbar."
		"Return the normal scrollbar fillStyle for the given scrollbar."
	
	"^ (self glamorousNormalFillStyleWithBaseColor: aScrollbar paneColor for:  aScrollbar height: aScrollbar height)
		direction: (aScrollbar bounds isWide
			ifTrue: [0 @ aScrollbar height]
			ifFalse: [aScrollbar width @ 0])"
"	^ SolidFillStyle color: Color lightGray lighter."
	^ self glamorousSelectedFillStyleFor: aScrollbar height: aScrollbar height
]
