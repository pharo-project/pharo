"
The theme is developed in the context of the Glamour project, and its goal is to create a look that:

- does not look like a specific operating system. In particular, the icons should be operating system agnostic, because, for example, people in Windows are confused by the red, yellow, green buttons of apple.
- uses a limited amount of colors and effects.
- is fast.
"
Class {
	#name : #PharoLightTheme,
	#superclass : #UITheme,
	#category : #'Polymorph-Widgets-Themes'
}

{ #category : #configuration }
PharoLightTheme class >> colorConfiguratorClass [

	^ LightThemeColorConfigurator
]

{ #category : #testing }
PharoLightTheme class >> isAbstract [
	"Answer whether the receiver is considered to be abstract."

	^false
]

{ #category : #settings }
PharoLightTheme class >> setPreferredShoutColors [
	"self setPreferredShoutColors"

	SHRBTextStyler styleTable:
		#(
			"(symbol color [emphasisSymbolOrArray [textStyleName [pixelHeight]]])"
			(default 								black)
			(invalid 									red)
			(comment 								(gray darker))
			(character 								(red muchDarker))
			(number 								(red muchDarker))
			(symbol 								(magenta muchDarker))
			(string 									(magenta muchDarker) normal)
			(selector 								(black))
			(incompleteSelector 					red)
			(undefinedSelector 					red)
			(patternSelector 						nil bold)
			(#self 									(cyan muchDarker ))
			(#super 								(cyan muchDarker ))
			(#true 									(red muchDarker))
			(#false 									(red muchDarker))
			(#nil 									(red muchDarker))
			(#thisContext 							(cyan muchDarker ))
			(#return 								(cyan muchDarker ) bold)
			(patternArg 							(blue muchDarker))
			(blockPatternArg 						(blue muchDarker))
			(blockArg 								(blue muchDarker))
			(argument 								(blue muchDarker))
			(blockArgColon 						black)
			(parenthesis 						black)
			(parenthesis1 						(green muchDarker))
			(parenthesis2 						(magenta muchDarker))
			(parenthesis3 						(red muchDarker))
			(parenthesis4 						(green darker))
			(parenthesis5 						(orange darker))
			(parenthesis6 						(magenta darker))
			(parenthesis7 						blue)
			(block 								black)
			(block1 							(green muchDarker))
			(block2 							(magenta muchDarker))
			(block3 							(red muchDarker))
			(block4 							(green darker))
			(block5 							(orange darker))
			(block6 							(magenta darker))
			(block7 							blue)
			(brace 								black)
			(brace1 								(green muchDarker))
			(brace2 								(magenta muchDarker))
			(brace3 								(red muchDarker))
			(brace4 								(green darker))
			(brace5 								(orange darker))
			(brace6 								(magenta darker))
			(brace7 								blue)
			(cascadeSeparator 					black)
			(statementSeparator 					black)
			(methodTempBar 						(black))
			(blockTempBar 						(black))
			(blockArgsBar 							(black))
			(pragma 						(green muchDarker))
			(blockTempVar 						(blue muchDarker))
			(blockPatternTempVar 					(blue muchDarker))
			(instVar 								(blue muchDarker))
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

{ #category : #'border-styles - buttons' }
PharoLightTheme >> buttonNormalBorderStyleFor: aButton [
	"Return the normal button borderStyle for the given button."

	| outerColor |
	(aButton valueOfProperty: #noBorder ifAbsent: [false])
		ifTrue: [ ^ SimpleBorderStyle new
							width: 0;
							baseColor: Color transparent ].
	outerColor := self darkBaseColorFor: aButton.
	^ SimpleBorderStyle new
			width: 1;
			baseColor: outerColor
]

{ #category : #'border-styles' }
PharoLightTheme >> configureWindowBorderFor: aWindow [

	| aStyle |
	aStyle := SimpleBorderStyle new
					color: (Color lightGray);
					width: 1.
	aWindow borderStyle: aStyle
]

{ #category : #initialization }
PharoLightTheme >> initialize [
	"self beCurrent"

	super initialize.
	self windowActiveDropShadowStyle: #diffuse.
	settings preferGradientFill: false
]

{ #category : #'fill-styles - scrollbars' }
PharoLightTheme >> scrollbarPressedThumbFillStyleFor: aScrollbar [
	"Return the pressed scrollbar thumb fillStyle for the given scrollbar."
	"Return the normal scrollbar fillStyle for the given scrollbar."
	"^ SolidFillStyle color: Color lightGray lighter."

	^ self selectedFillStyleFor: aScrollbar height: aScrollbar height
]

{ #category : #'fill-styles - taskbar' }
PharoLightTheme >> taskbarFillStyleFor: aTaskbar [
	^ SolidFillStyle color: (Color gray muchLighter alpha: 0.8)
]

{ #category : #'border-styles - taskbar' }
PharoLightTheme >> taskbarItemDisabledBorderStyleFor: aButton [
	^ self taskbarItemNormalBorderStyleFor: aButton
]

{ #category : #'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemDisabledFillStyleFor: aButton [
	"Return the disabled taskbar item fillStyle for the given color."

	^ self taskbarItemNormalFillStyleFor: aButton
]

{ #category : #'border-styles - taskbar' }
PharoLightTheme >> taskbarItemMouseOverBorderStyleFor: aButton [
	^ BottomBorderStyle new
		color: (Color fromHexString: '616161');
		width: 2;
		yourself
]

{ #category : #'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemMouseOverFillStyleFor: aButton [
	^ SolidFillStyle color: (Color black alpha: 0.07)
]

{ #category : #'border-styles - taskbar' }
PharoLightTheme >> taskbarItemNormalBorderStyleFor: aButton [
	^ BottomBorderStyle new
		color: (Color fromHexString: '757575');
		width: 2;
		trimSize: 2;
		yourself
]

{ #category : #'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemNormalFillStyleFor: aButton [
	^ SolidFillStyle color: Color transparent
]

{ #category : #'border-styles - taskbar' }
PharoLightTheme >> taskbarItemPressedBorderStyleFor: aButton [
	^ self taskbarItemNormalBorderStyleFor: aButton
]

{ #category : #'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemPressedFillStyleFor: aButton [
	^ SolidFillStyle color: (Color black alpha: 0.15)
]

{ #category : #'border-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedBorderStyleFor: aButton [
	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : #'border-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedDisabledBorderStyleFor: aButton [
	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : #'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedFillStyleFor: aButton [
	^ SolidFillStyle color: (Color black alpha: 0.1)
]

{ #category : #'border-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedMouseOverBorderStyleFor: aButton [
	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : #'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedMouseOverFillStyleFor: aButton [
	^ SolidFillStyle color: (Color black alpha: 0.15)
]

{ #category : #'border-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedPressedBorderStyleFor: aButton [
	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : #'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedPressedFillStyleFor: aButton [
	^ SolidFillStyle color: (Color black alpha: 0.2)
]
