"
The theme is developed in the context of the Glamour project, and its goal is to create a look that:
- does not look like a specific operating system.
- maximizes whitespace.
- is fast.

!!Installation of the theme

[[[
	PolymorphSystemSettings desktopColor: Color white.
	self defaultSettings: nil.
	self beCurrent.
	self setPreferredShoutColors
]]]


"
Class {
	#name : #GLMWhitespaceTheme,
	#superclass : #UITheme,
	#category : 'Glamour-Morphic-Theme'
}

{ #category : #accessing }
GLMWhitespaceTheme class >> baseColor [
	^ Color white
]

{ #category : #accessing }
GLMWhitespaceTheme class >> basePassiveBackgroundColor [
	^ Color r: 245 g: 245 b: 245 range: 255
]

{ #category : #accessing }
GLMWhitespaceTheme class >> baseSelectionColor [
	^ Color r: 97 g: 163 b: 225 range: 255
]

{ #category : #accessing }
GLMWhitespaceTheme class >> darkBaseColor [
	^ Color r: 210 g: 210 b: 210 range: 255
]

{ #category : #private }
GLMWhitespaceTheme class >> importGlamorousIcons [
	"utility method to import the icons necessary for the theme from the file system"
	"self importGlamorousIcons"
	| icons |
	icons := #(
		'glamorousMax'
		'glamorousMin'
		'glamorousClose'
		'glamorousMenu'
		'glamorousMaxInactive'
		'glamorousMinInactive'
		'glamorousCloseInactive'
		'glamorousMenuInactive'
		'glamorousMenuPin'
		'glamorousCheckboxSelected'
		'glamorousCheckboxUnselected'
		'glamorousRadioSelected'
		'glamorousRadioUnselected').
	self flag: 'Commented this as GLMThemeIcons was removed.'
	"self importIcons: icons fromFolder: 'icons' inClass: GLMThemeIcons category: '*glamour-morphic-theme'"
]

{ #category : #accessing }
GLMWhitespaceTheme class >> isAbstract [
	"Answer whether the receiver is considered to be abstract."

	^false
]

{ #category : #accessing }
GLMWhitespaceTheme class >> lightBaseColor [
	^ Color r: 220 g: 220 b: 220 range: 255
]

{ #category : #accessing }
GLMWhitespaceTheme class >> lightSelectionColor [
	^ Color r: 175 g: 213 b: 250 range: 255
]

{ #category : #settings }
GLMWhitespaceTheme class >> newDefaultSettings [
	
	BalloonMorph setBalloonColorTo: self veryLightSelectionColor.
	^super newDefaultSettings
]

{ #category : #settings }
GLMWhitespaceTheme class >> setPreferredPreferences [
	NECPreferences 
		expandPrefixes: true;
		popupShowWithShortcut: Character tab asShortcut.
]

{ #category : #accessing }
GLMWhitespaceTheme class >> themeName [
	^ 'Glamorous Whitespace'
]

{ #category : #accessing }
GLMWhitespaceTheme class >> veryLightSelectionColor [
	^ Color white darker
]

{ #category : #'accessing colors' }
GLMWhitespaceTheme >> borderColor [ 
	^ Color transparent
]

{ #category : #label-styles }
GLMWhitespaceTheme >> buttonLabelForText: aTextOrString [
	"Answer the label to use for the given text."
	^aTextOrString isString
		ifTrue: [(LabelMorph contents: aTextOrString) color: Color black]
		ifFalse: [super buttonLabelForText: aTextOrString]
]

{ #category : #border-styles-buttons }
GLMWhitespaceTheme >> buttonNormalBorderStyleFor: aButton [
	"Return the normal button borderStyle for the given button."

	(aButton valueOfProperty: #noBorder ifAbsent: [false]) ifTrue: [
		^ SimpleBorder new width: 0; baseColor: Color transparent ].
	^ self glamorousNoBorderFor: aButton
]

{ #category : #fill-styles-buttons }
GLMWhitespaceTheme >> buttonSelectedFillStyleFor: aButton [
	"Return the normal button fillStyle for the given button."
	
	| top bottom |
	top := self glamorousLightSelectionColorFor: aButton.
	bottom := self glamorousBaseColorFor: aButton.

	^(GradientFillStyle ramp: {
			0.0->top.
			0.7->bottom.})
		origin: aButton bounds origin;
		direction: 0 @ aButton height;
		radial: false
]

{ #category : #'watcher window' }
GLMWhitespaceTheme >> configureWatcherWindowLabelAreaFor: aWindow [
	"Configure the label area for the given Watcher window."

	|padding|
	padding := 0.
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0).
	aWindow hasCloseBox ifTrue: [aWindow addCloseBox. padding := padding + 1].
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0; hResizing: #spaceFill).
	aWindow basicLabel ifNotNil: [:label | aWindow labelArea addMorphBack: label; hResizing: #shrinkWrap].
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0; hResizing: #spaceFill).
	padding > 0 ifTrue: [
		aWindow labelArea
			addMorphBack: (Morph new extent: (aWindow boxExtent x * padding) @ 0)].
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0)
]

{ #category : #border-styles }
GLMWhitespaceTheme >> configureWindowBorderFor: aWindow [
	^ self glamorousThickBorderFor: aWindow
]

{ #category : #label-styles }
GLMWhitespaceTheme >> configureWindowLabelAreaFor: aWindow [
	"Configure the label area for the given window."

	|padding|
	padding := 0.
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0).
	aWindow hasCloseBox ifTrue: [aWindow addCloseBox. padding := padding + 1].
	aWindow hasCollapseBox ifTrue: [aWindow addCollapseBox. padding := padding + 1].
	aWindow hasExpandBox ifTrue: [aWindow addExpandBox. padding := padding + 1].
	aWindow hasMenuBox ifTrue: [padding := padding - 1].
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0; hResizing: #spaceFill).
	aWindow basicLabel ifNotNil: [:label | aWindow labelArea addMorphBack: label; hResizing: #shrinkWrap].
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0; hResizing: #spaceFill).
	padding > 0 ifTrue: [
		aWindow labelArea
			addMorphBack: (Morph new extent: (aWindow boxExtent x * padding) @ 0)].
	aWindow hasMenuBox ifTrue: [aWindow addMenuControl].
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0)
]

{ #category : #fill-styles }
GLMWhitespaceTheme >> desktopColor [

	^(Color r: 250 g: 250 b: 250 range: 256)
]

{ #category : #defaults }
GLMWhitespaceTheme >> dialogWindowPreferredCornerStyleFor: aDialogWindow [
	"Answer the preferred corner style for the given dialog."

	^#square
]

{ #category : #border-styles }
GLMWhitespaceTheme >> dropListNormalBorderStyleFor: aDropList [
	"Return the normal borderStyle for the given drop list"

	^ self glamorousLightBorderStyleFor: aDropList
]

{ #category : #private }
GLMWhitespaceTheme >> glamorousBaseColorFor: aButton [
	^ self class baseColor
	"unfortunately, it looks like paneColor does not always return the wanted color"
	"aButton paneColorOrNil ifNil: [Color r: 200 g: 200 b: 200 range: 255]"
]

{ #category : #private }
GLMWhitespaceTheme >> glamorousBasePassiveBackgroundColorFor: aButton [
	^ self class basePassiveBackgroundColor
]

{ #category : #private }
GLMWhitespaceTheme >> glamorousBaseSelectionColorFor: aButton [
	^ self class baseSelectionColor
]

{ #category : #private }
GLMWhitespaceTheme >> glamorousDarkBaseColorFor: aButton [
	^ self class darkBaseColor
]

{ #category : #private }
GLMWhitespaceTheme >> glamorousLightBorderStyleFor: aTextEditor [
	^ SimpleBorder new
		baseColor: (self glamorousLightColorFor: aTextEditor);
		width: 1
]

{ #category : #private }
GLMWhitespaceTheme >> glamorousLightColorFor: aButton [
	^ self class lightBaseColor
]

{ #category : #private }
GLMWhitespaceTheme >> glamorousLightSelectionColorFor: aMorph [
	^ self class lightSelectionColor
]

{ #category : #private }
GLMWhitespaceTheme >> glamorousNoBorderFor: aMorph [
	| aStyle |
	aStyle := SimpleBorder new 
			baseColor: (self glamorousLightColorFor: aMorph);
			width: 0.
	^ aStyle
]

{ #category : #private }
GLMWhitespaceTheme >> glamorousNormalFillStyleFor: aMorph height: anInteger [
	"Return the normal button fillStyle for the given morph."

	^ SolidFillStyle color: (self glamorousLightColorFor: aMorph)
]

{ #category : #private }
GLMWhitespaceTheme >> glamorousNormalFillStyleWithBaseColor: aColor for: aMorph height: anInteger [	
	| top bottom |
	top := aColor "darker".
	bottom := aColor.
	^(GradientFillStyle ramp: {
			0.0->top.
			0.7->bottom.})
		origin: aMorph bounds origin;
		direction: 0 @ anInteger;
		radial: false
]

{ #category : #private }
GLMWhitespaceTheme >> glamorousThickBorderFor: aWindow [
	| aStyle desiredColor |
	"Color lightGray is the color with which SystemWindow initializes the border color.
	If it is the default, we override it with the theme specific color.
	Otherwise, we choose the desired color"
	desiredColor := 
		(aWindow borderColor isNil or: [
			aWindow borderColor isColor not or: [ "Prevent crashes for #raised or #inset borders"
				aWindow borderColor = Color lightGray]])
							ifTrue: [ self glamorousDarkBaseColorFor: aWindow ]
							ifFalse: [ aWindow borderColor ].
	aStyle := SimpleBorder new
					baseColor: desiredColor;
					width: 2.
	aWindow borderStyle: aStyle.
	^ aStyle
]

{ #category : #private }
GLMWhitespaceTheme >> glamorousVisibleFillStyleFor: aMorph height: anInteger [
	"Return the normal button fillStyle for the given morph."

	^ SolidFillStyle color: (self glamorousLightColorFor: aMorph)
]

{ #category : #border-styles }
GLMWhitespaceTheme >> groupPanelBorderStyleFor: aGroupPanel [
	"Answer the normal border style for a group panel."

	^ self glamorousNoBorderFor: aGroupPanel
]

{ #category : #'growl - specific' }
GLMWhitespaceTheme >> growlBorderColorFor: aGrowlMorph [
	^ self glamorousThickBorderFor: aGrowlMorph 
]

{ #category : #'growl - specific' }
GLMWhitespaceTheme >> growlContentsColorFor: aGrowlMorph [
	^ Color black
]

{ #category : #'growl - specific' }
GLMWhitespaceTheme >> growlFillColorFor: aGrowlMorph [
	^ Color white darker
]

{ #category : #'growl - specific' }
GLMWhitespaceTheme >> growlLabelColorFor: aGrowlMorph [
	^ Color black
]

{ #category : #initialize-release }
GLMWhitespaceTheme >> initialize [
	"self beCurrent"

	super initialize.
	self windowActiveDropShadowStyle: #diffuse
]

{ #category : #border-styles }
GLMWhitespaceTheme >> listNormalBorderStyleFor: aList [
	"Return the normal borderStyle for the given list"
		
	^ self glamorousNoBorderFor: aList
]

{ #category : #fill-styles }
GLMWhitespaceTheme >> morphTreeSplitterNormalFillStyleFor: aSplitter [	
	^ SolidFillStyle color: Color transparent
]

{ #category : #'morph creation' }
GLMWhitespaceTheme >> newFocusIndicatorMorphFor: aMorph [
	"Answer a new focus indicator for the given morph."

	|radius|
	radius := aMorph focusIndicatorCornerRadius.
	^ BorderedMorph new
		fillStyle: Color transparent;
		borderStyle: (SimpleBorder new
			 width: 1;
			baseColor: (self glamorousBaseSelectionColorFor: aMorph));
		bounds: aMorph focusBounds
]

{ #category : #initialize-release }
GLMWhitespaceTheme >> newRadioMarkerForm [
	"Answer a new checkbox marker form."

	^Form extent: 12@12 depth: 32
]

{ #category : #initialize-release }
GLMWhitespaceTheme >> newTreeExpandedForm [
	"Answer a new form for an expanded tree item."

	^(Form
	extent: 9@9
	depth: 32
	fromArray: #( 1049135240 2290649224 2290649224 2290649224 2290649224 2290649224 2290649224 2290649224 1200130184 478709896 4169697416 4287137928 4287137928 4287137928 4287137928 4287137928 4236806280 646482056 16777215 2508753032 4287137928 4287137928 4287137928 4287137928 4287137928 2726856840 16777215 16777215 495487112 4186474632 4287137928 4287137928 4287137928 4236806280 612927624 16777215 16777215 16777215 2542307464 4287137928 4287137928 4287137928 2676525192 16777215 16777215 16777215 16777215 478709896 4169697416 4287137928 4220029064 579373192 16777215 16777215 16777215 16777215 16777215 2424866952 4287137928 2626193544 16777215 16777215 16777215 16777215 16777215 16777215 394823816 4018702472 529041544 16777215 16777215 16777215 16777215 16777215 16777215 16777215 864585864 16777215 16777215 16777215 16777215)
	offset: 0@0)
]

{ #category : #initialize-release }
GLMWhitespaceTheme >> newTreeUnexpandedForm [
	"Answer a new form for an unexpanded tree item."

	^(Form
	extent: 9@9
	depth: 32
	fromArray: #( 1049135240 461932680 16777215 16777215 16777215 16777215 16777215 16777215 16777215 2324203656 4152920200 2458421384 428378248 16777215 16777215 16777215 16777215 16777215 2357758088 4287137928 4287137928 4152920200 2408089736 394823816 16777215 16777215 16777215 2391312520 4287137928 4287137928 4287137928 4287137928 4119365768 2324203656 344492168 16777215 2408089736 4287137928 4287137928 4287137928 4287137928 4287137928 4287137928 3968370824 780699784 2391312520 4287137928 4287137928 4287137928 4287137928 4236806280 2659747976 529041544 16777215 2357758088 4287137928 4287137928 4253583496 2810742920 646482056 16777215 16777215 16777215 2324203656 4253583496 2777188488 696813704 16777215 16777215 16777215 16777215 16777215 1200130184 663259272 16777215 16777215 16777215 16777215 16777215 16777215 16777215)
	offset: 0@0)
]

{ #category : #initialize-release }
GLMWhitespaceTheme >> newWindowInactiveControlForm [
	"Answer a new form for an inactive window control box."

	^(Form
	extent: 16@16
	depth: 32
	fromArray: #( 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4291677645 4288585374 4286085240 4284243036 4284243036 4286085240 4288585374 4291677645 0 0 0 0 0 0 0 4289572269 4285756275 4286479998 4288716960 4289835441 4289835441 4288716960 4286479998 4285756275 4289572269 0 0 0 0 0 4289506476 4284835173 4287335307 4290559164 4292598747 4293322470 4293322470 4292598747 4290559164 4287335307 4284703587 4289506476 0 0 0 4291546059 4285493103 4286414205 4288980132 4291217094 4292335575 4292598747 4292598747 4292335575 4291282887 4288980132 4286282619 4285493103 4291546059 0 0 4288980132 4285361517 4287466893 4288782753 4289835441 4290295992 4290295992 4290427578 4290164406 4289835441 4288782753 4287466893 4285361517 4288980132 0 0 4286282619 4286611584 4288059030 4288716960 4289177511 4289572269 4289835441 4289835441 4289703855 4289374890 4288782753 4288059030 4286611584 4286282619 0 0 4285164138 4287664272 4288782753 4289374890 4289835441 4290427578 4290624957 4290624957 4290559164 4290032820 4289374890 4288914339 4287664272 4285164138 0 0 4285361517 4288322202 4289703855 4290295992 4290822336 4291414473 4291677645 4291677645 4291414473 4291085508 4290427578 4289703855 4288453788 4285624689 0 0 4287072135 4288716960 4290427578 4291217094 4291677645 4292203989 4292598747 4292598747 4292335575 4291809231 4291217094 4290427578 4288716960 4287203721 0 0 4288980132 4288256409 4290624957 4291677645 4292335575 4292927712 4293256677 4293256677 4293059298 4292598747 4291809231 4290822336 4288256409 4289177511 0 0 4291677645 4287664272 4290295992 4292006610 4293059298 4293454056 4293585642 4293585642 4293454056 4293125091 4292203989 4290427578 4287730065 4291677645 0 0 4293256677 4290032820 4288124823 4291217094 4292796126 4293322470 4293717228 4293717228 4293454056 4292927712 4291677645 4288256409 4290032820 4293256677 0 0 0 4293454056 4290032820 4288322202 4289967027 4291546059 4292598747 4292664540 4291677645 4290295992 4288716960 4290032820 4293454056 0 0 0 0 0 4293322470 4292203989 4289835441 4288782753 4288322202 4288453788 4288980132 4289835441 4292335575 4293322470 0 0 0 0 0 0 0 4293059298 4293585642 4293717228 4293585642 4293585642 4293585642 4293585642 4293059298 0 0 0 0)
	offset: 0@0)
]

{ #category : #border-styles }
GLMWhitespaceTheme >> plainGroupPanelBorderStyleFor: aGroupPanel [
	"Answer the normal border style for a plain group panel."

	^SimpleBorder new
		width: 1;
		baseColor: Color transparent
]

{ #category : #fill-styles }
GLMWhitespaceTheme >> resizerGripNormalFillStyleFor: aResizer [
	"Return the normal fillStyle for the given resizer.
	For the moment, answer a transparent colour for no drawing,
	non transparent to draw as normal."
	self flag: 'this is used for the edge grip'.
	^ super resizerGripNormalFillStyleFor: aResizer
	
"	aResizer 
		instVarNamed: #handleColor put: (self glamorousDarkBaseColorFor:  aResizer);
		instVarNamed: #dotColor put: Color transparent.
	
	^SolidFillStyle color: Color red"
]

{ #category : #themers }
GLMWhitespaceTheme >> rubScrolledTextMorphThemer [
	^ RubScrolledTextMorphThemerForGLMWhitespaceTheme new
]

{ #category : #scrollbars }
GLMWhitespaceTheme >> scrollBarButtonArrowVertices: aRectangle [ 
	^ self verticesForComplexArrow: aRectangle
]

{ #category : #scrollbars }
GLMWhitespaceTheme >> scrollbarArrowOfDirection: aSymbol size: finalSizeInteger color: aColor [
	^ Form extent: 1@1
]

{ #category : #fill-styles-scrollbars }
GLMWhitespaceTheme >> scrollbarNormalButtonFillStyleFor: aScrollbar [
	"Return the normal scrollbar button fillStyle for the given scrollbar."
	
	^ self 
		glamorousNormalFillStyleFor: aScrollbar 
		height: aScrollbar height
]

{ #category : #fill-styles-scrollbars }
GLMWhitespaceTheme >> scrollbarNormalFillStyleFor: aScrollbar [
	"Return the normal scrollbar fillStyle for the given scrollbar."
	
	^ Color transparent
]

{ #category : #defaults }
GLMWhitespaceTheme >> shStyleTable [

	^#(
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
			(incompleteIdentifier 				(gray muchDarker))
			(incompleteKeyword 					(gray muchDarker))
			(incompleteBinary 						(gray muchDarker)) 
			(incompleteUnary 						(gray muchDarker))
			(undefinedIdentifier 					red)
			(undefinedKeyword 						red)
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
			(tempVar 								(blue muchDarker))
			(patternTempVar 						(blue muchDarker))
			(poolConstant 							(blue muchDarker))
			(classVar 								(blue muchDarker))
			(globalVar 								(blue muchDarker)))

]

{ #category : #fill-styles }
GLMWhitespaceTheme >> splitterNormalFillStyleFor: aSplitter [
	"Return the normal splitter fillStyle for the given splitter."
	
	^ self 
		glamorousNormalFillStyleFor: aSplitter 
		height: 1
]

{ #category : #fill-styles }
GLMWhitespaceTheme >> splitterPressedFillStyleFor: aSplitter [
	"Return the pressed splitter fillStyle for the given splitter."
	
	^ self splitterNormalFillStyleFor: aSplitter
]

{ #category : #basic-colors }
GLMWhitespaceTheme >> subgroupColorFrom: paneColor [
	"Answer the colour for a subgroup given the pane colour."

	^ self glamorousBaseColorFor: paneColor
]

{ #category : #border-styles }
GLMWhitespaceTheme >> tabPanelBorderStyleFor: aTabGroup [
	^ GLMTabPanelBorder new 
		width: 1;
		baseColor: (self glamorousLightColorFor: aTabGroup);
		tabSelector: aTabGroup tabSelectorMorph
]

{ #category : #border-styles }
GLMWhitespaceTheme >> textEditorNormalBorderStyleFor: aTextEditor [
	"Return the normal text editor borderStyle for the given text editor."

	^ self glamorousLightBorderStyleFor: aTextEditor
]

{ #category : #fill-styles }
GLMWhitespaceTheme >> windowActiveFillStyleFor: aWindow [
	"We do not want the lighting effect when the window goes inactive"
	"^ SolidFillStyle color: (self class lightBaseColor)"
	
	^ SolidFillStyle color: (self glamorousBaseColorFor: aWindow)
]

{ #category : #fill-styles }
GLMWhitespaceTheme >> windowActiveTitleFillStyleFor: aWindow [
	^ SolidFillStyle color: (self glamorousDarkBaseColorFor: aWindow)
]

{ #category : #label-styles }
GLMWhitespaceTheme >> windowCloseOverForm [
	"Answer the form to use for mouse over window close buttons"

	^self forms at: #windowCloseOver ifAbsent: [Form extent: 16@16 depth: Display depth]
]

{ #category : #fill-styles }
GLMWhitespaceTheme >> windowEdgeNormalFillStyleFor: anEdgeGrip [
	"Return the normal window edge fillStyle for the given edge grip."
	self flag: 'this is used for the long edges'.
	^ super windowEdgeNormalFillStyleFor: anEdgeGrip
	"self glamorousDarkBaseColorFor: anEdgeGrip"
]

{ #category : #defaults }
GLMWhitespaceTheme >> windowShadowColor [
	"Answer the window shadow color to use."

	^ Color transparent
]
