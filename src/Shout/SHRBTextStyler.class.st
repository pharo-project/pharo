"
I'm the one who visits node to coloring the code
"
Class {
	#name : 'SHRBTextStyler',
	#superclass : 'RBProgramNodeVisitor',
	#instVars : [
		'charAttr',
		'parentheseLevel',
		'bracketLevel',
		'classOrMetaClass',
		'workspace',
		'plugins',
		'braceLevel',
		'view',
		'stylingEnabled',
		'text',
		'isScripting'
	],
	#classInstVars : [
		'styleTable',
		'formatIncompleteIdentifiers',
		'textAttributes'
	],
	#category : 'Shout-Styling',
	#package : 'Shout',
	#tag : 'Styling'
}

{ #category : 'attributes' }
SHRBTextStyler class >> attributeArrayForColor: aColorOrNil emphasis: anEmphasisSymbolOrArrayorNil [

	| emphArray answer |
	answer := Array new.
	aColorOrNil ifNotNil: [ answer := answer , { (TextColor color: aColorOrNil) } ].

	anEmphasisSymbolOrArrayorNil ifNotNil: [
		emphArray := anEmphasisSymbolOrArrayorNil isSymbol
			             ifTrue: [ { anEmphasisSymbolOrArrayorNil } ]
			             ifFalse: [ anEmphasisSymbolOrArrayorNil ].
		emphArray do: [ :each | each ~= #normal ifTrue: [ answer := answer , { (TextEmphasis perform: each) } ] ] ].

	^ answer
]

{ #category : 'private' }
SHRBTextStyler class >> attributesFor: aSymbol [

	^ self textAttributes at: aSymbol ifAbsent: [ #(  ) ]
]

{ #category : 'styles' }
SHRBTextStyler class >> blueStyleTable [
	"color can be a valid argument to Color class>>colorFrom: , or nil to
	use the editor text color.
	Multiple emphases can be specified using an array e.g. #(bold italic).
	If emphasis is not specified, #normal will be used.
	if pixel height is not specified , then the editor font size will be used.
	"
	<styleTable: 'Blue'>

			"(symbol color [emphasisSymbolOrArray [textStyleName [pixelHeight]]])"
 ^ #(
			(default 								black)
			(invalid 									red)

			(comment 								(gray darker))

			(character 								(red muchDarker))
			(number 								(red muchDarker))
			(symbol 								(magenta muchDarker))
			(string 									(magenta muchDarker) normal)

			(selector 								(black))

			(#self 									(cyan muchDarker ))
			(#super 								(cyan muchDarker ))
			(#true 									(red muchDarker))
			(#false 									(red muchDarker))
			(#nil 									(red muchDarker))
			(#thisContext 							(cyan muchDarker ))
			(#thisProcess 							(cyan muchDarker ))
			(#return 								(cyan muchDarker ) bold)
			(patternArg 							(blue muchDarker))
			(blockPatternArg 						(blue muchDarker))
			(blockArg 								(blue muchDarker))
			(argument 								(blue muchDarker))
			(blockTempVar 						(blue muchDarker))
			(blockPatternTempVar 					(blue muchDarker))
			(instVar 								(cyan muchDarker))

			(tempVar 								(blue muchDarker))
			(patternTempVar 						(blue muchDarker))
			(poolConstant 							(cyan muchDarker))
			(classVar 								(cyan muchDarker))
			(globalVar 								(purple muchDarker))

			(incompleteIdentifier 					blue italic)
			(incompleteSelector 					nil  italic)

			(undefinedIdentifier 					red)
			(undefinedSelector 					red)

			(patternSelector 						nil bold)
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

			(pragma 						(green muchDarker)))
]

{ #category : 'styles' }
SHRBTextStyler class >> darkStyleTable [
	"color can be a valid argument to Color class>>colorFrom: , or nil to
	use the editor text color.
	Multiple emphases can be specified using an array e.g. #(bold italic).
	If emphasis is not specified, #normal will be used.
	if pixel height is not specified , then the editor font size will be used.

	This style is inspired on darkula theme from eclipse: http://eclipsecolorthemes.org/?view=theme&id=15515.
	"
	<styleTable: 'Dark'>

			"(symbol color [emphasisSymbolOrArray [textStyleName [pixelHeight]]])"
 ^ #(
			(default 								white)
			(invalid 								'FF8A80')

			(comment 								'7D8C93')

			(character 							('E38C1E' lighter))
			(number 								('FFE54B'))
			(symbol 								('E38C1E'))
			(string 								('E38C1E'))

			(selector 								white)

			(#true 								('00CC6E' darker))
			(#false 								('00CC6E' darker))
			(#nil 									('00CC6E' darker))

			(#self 								'00CC6E')
			(#super								'00CC6E')
			(#thisContext 						'00CC6E')
			(#thisProcess 						'00CC6E')
			(#return 								'00CC6E' bold)

			(patternArg 							'A7E2ED')
			(blockPatternArg 					'A7E2ED' italic)
			(blockArg 							'A7E2ED' italic)
			(argument 							'A7E2ED')
			(blockTempVar 						'A7E2ED')
			(blockPatternTempVar 				'A7E2ED')

			(instVar 								'A7E2ED')

			(tempVar 								'A7E2ED')
			(patternTempVar 					'A7E2ED')

			(poolConstant 						'A7E2ED' bold)
			(classVar 							'A7E2ED' bold)
			(globalVar 							white bold)


			(incompleteIdentifier 			'E8E2B7' italic)
			(incompleteSelector 				'E8E2B7' italic)

			(undefinedIdentifier 				'FF8A80')
			(undefinedSelector 					'FF8A80')

			(patternSelector 					nil bold)
			(blockArgColon 						white)
			(parenthesis 					white)
			(parenthesis1 					(green muchLighter))
			(parenthesis2 					(magenta muchLighter))
			(parenthesis3 					('FF8A80' muchLighter))
			(parenthesis4 					(green lighter))
			(parenthesis5 					(orange lighter))
			(parenthesis6 					(magenta lighter))
			(parenthesis7 					blue)
			(block 							white)
			(block1 						(green muchLighter))
			(block2 						(magenta muchLighter))
			(block3 						(red muchLighter))
			(block4 						(green lighter))
			(block5 						(orange lighter))
			(block6 						(magenta lighter))
			(block7 						blue)
			(brace 							white)
			(brace1 						(green muchLighter))
			(brace2 						(magenta muchLighter))
			(brace3 						(red muchLighter))
			(brace4 						(green lighter))
			(brace5 						(orange lighter))
			(brace6 						(magenta lighter))
			(brace7 						blue)
			(cascadeSeparator 					white bold)
			(statementSeparator 				white bold)
			(methodTempBar 						white)
			(blockTempBar 						white)
			(blockArgsBar 						white)

			(pragma 									'B4DD6E'))
]

{ #category : 'styles' }
SHRBTextStyler class >> defaultStyleTable [
	"color can be a valid argument to Color class>>colorFrom: , or nil to
	use the editor text color.
	Multiple emphases can be specified using an array e.g. #(bold italic).
	If emphasis is not specified, #normal will be used.
	if pixel height is not specified , then the editor font size will be used.
	"

 	^ self blueStyleTable
]

{ #category : 'styles' }
SHRBTextStyler class >> formatIncompleteIdentifiers [

	^ formatIncompleteIdentifiers ifNil: [ formatIncompleteIdentifiers := false ]
]

{ #category : 'styles' }
SHRBTextStyler class >> formatIncompleteIdentifiers: aValue [

	formatIncompleteIdentifiers := aValue
]

{ #category : 'attributes' }
SHRBTextStyler class >> initialTextAttributes [

	^ self initialTextAttributesForStyleTable: self styleTable
]

{ #category : 'attributes' }
SHRBTextStyler class >> initialTextAttributesForStyleTable: aStyleTable [

	| d |
	d := IdentityDictionary new.
	aStyleTable do: [ :each |
		| element emphasis color |
		element := each first.
		color := each at: 2 ifAbsent: [  ].
		color := color ifNotNil: [ Color colorFrom: color ].
		emphasis := each at: 3 ifAbsent: [  ].

		(self attributeArrayForColor: color emphasis: emphasis) ifNotEmpty: [ :attrArray | d at: element put: attrArray ] ].

	^ d
]

{ #category : 'styles' }
SHRBTextStyler class >> newAttributesForStyleTable: aStyleTable [
	
	^ self initialTextAttributesForStyleTable: aStyleTable
]

{ #category : 'styles' }
SHRBTextStyler class >> newAttributesForStyleTableNamed: aString [
	| table |
	
	table := self styleTableNamed: aString.
	^ self newAttributesForStyleTable: (table ifNil: [ self styleTable ])
]

{ #category : 'initialization' }
SHRBTextStyler class >> resetTextAttributesCaches [

	<script>
	textAttributes := nil
]

{ #category : 'styles' }
SHRBTextStyler class >> settingsOn: aBuilder [
	<systemsettings>

	(aBuilder setting: #formatIncompleteIdentifiers)
		target: self;
		default: false;
		order: 1;
		label: 'Format Incomplete Identifiers';
		parentName: #'Syntax Highlighting';
		description: 'If the code highlighter tryies to format incomplete identifiers and selectors or not. This is not recommended for big images, as it traverse all the image to get the information'
]

{ #category : 'styles' }
SHRBTextStyler class >> solarizedStyleTable [
	"color can be a valid argument to Color class>>colorFrom: , or nil to
	use the editor text color.
	Multiple emphases can be specified using an array e.g. #(bold italic).
	If emphasis is not specified, #normal will be used.
	if pixel height is not specified , then the editor font size will be used.

	This style is based on old theme and other old environments.
	"
	<styleTable: 'Solarized'>

			"(symbol color [emphasisSymbolOrArray [textStyleName [pixelHeight]]])"
 ^ #(
			(default 								('657A81' muchDarker))
			(invalid 									red)

			(comment 								'586E75')

			(character 								('268BD2' lighter))
			(number 								('2AA198'))
			(symbol 								('268BD2'))
			(string 									('268BD2'))


			(selector 								nil)

			(#true 									('CB4B16' darker))
			(#false 									('CB4B16' darker))
			(#nil 									('CB4B16' darker))

			(#self 									'B58900')
			(#super								'B58900')
			(#thisContext 							'B58900')
			(#thisProcess 							'B58900')
			(#return 								'B58900' bold)

			(patternArg 							('D33682'))
			(blockPatternArg 						('D33682'))
			(blockArg 								('D33682'))
			(argument 								('D33682'))
			(blockTempVar 						('D33682'))
			(blockPatternTempVar 					('D33682'))

			(instVar 								('D33682'))

			(tempVar 								('D33682'))
			(patternTempVar 						('D33682'))

			(poolConstant 							('D33682') bold)
			(classVar 								('D33682') bold)
			(globalVar 								('268BD2' muchDarker) bold)

			(undefinedIdentifier 					red)
			(incompleteIdentifier 					'D33682' italic)
			(incompleteSelector 					nil  italic)
			(undefinedSelector 					red)
			(patternSelector 						'657A81' bold)
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
			(cascadeSeparator 					black bold)
			(statementSeparator 					black bold)
			(methodTempBar 						black)
			(blockTempBar 						black)
			(blockArgsBar 							black)

			(pragma 						(cyan muchDarker)))
]

{ #category : 'accessing' }
SHRBTextStyler class >> styleTable [
	^ styleTable
		ifNil: [styleTable := self defaultStyleTable]
]

{ #category : 'accessing' }
SHRBTextStyler class >> styleTable: anArray [

	styleTable := anArray.
	self resetTextAttributesCaches
]

{ #category : 'styles' }
SHRBTextStyler class >> styleTableNamed: aString [

	^ (Pragma allNamed: #styleTable: in: self class)
		detect: [ :each | (each argumentAt: 1) = aString ]
		ifFound: [ :each | self perform: each methodSelector ]
		ifNone: [ nil ]
]

{ #category : 'styles' }
SHRBTextStyler class >> tangoStyleTable [
	"color can be a valid argument to Color class>>colorFrom: , or nil to
	use the editor text color.
	Multiple emphases can be specified using an array e.g. #(bold italic).
	If emphasis is not specified, #normal will be used.
	if pixel height is not specified , then the editor font size will be used.

	This is a syntax coloring scheme based on the tango desktop icon scheme:
	http://tango.freedesktop.org/Tango_Icon_Theme_Guidelines
	"
	<styleTable: 'Tango'>

			"(symbol color [emphasisSymbolOrArray [textStyleName [pixelHeight]]])"
 ^ #(
			(default 								black)
			(invalid 									red)

			(comment 								'888a85')

			(character 								'ce5c00')
			(number 								'8f5902')
			(symbol 								'c4a000')
			(string 									'ce5c00')


			(selector 								(black))

			(#true 									'8F5902')
			(#false 									'8F5902')
			(#nil 									'8F5902')

			(#self 									'4d9a06')
			(#super 								'4d9a06')
			(#thisContext 							'4d9a06')
			(#thisProcess 							'4d9a06')
			(#return 								black bold)

			(patternArg 							'2e3426' italic)
			(blockPatternArg 						'888a85' bold)
			(blockArg 								'555753' italic)
			(argument 								'555753' italic)
			(blockTempVar 						'555753' italic)
			(blockPatternTempVar 					'888a85' italic)

			(instVar 								'204a87')

			(tempVar 								'555753' italic)
			(patternTempVar 						'888a85' italic)

			(poolConstant 							'ad7fa8' bold)
			(classVar 								'ad7fa8' bold)
			(globalVar 								'5c3566' bold)


			(incompleteIdentifier 					blue italic)
			(incompleteSelector 					blue italic)

			(undefinedIdentifier 					red)
			(undefinedSelector 					red)
			(patternSelector 						nil bold)
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
			(methodTempBar 						black)
			(blockTempBar 						black)
			(blockArgsBar 							black)

			(pragma 						(green muchDarker)))
]

{ #category : 'accessing' }
SHRBTextStyler class >> textAttributes [

	^ textAttributes ifNil: [ textAttributes := self initialTextAttributes ]
]

{ #category : 'styles' }
SHRBTextStyler class >> vintageStyleTable [
	"color can be a valid argument to Color class>>colorFrom: , or nil to
	use the editor text color.
	Multiple emphases can be specified using an array e.g. #(bold italic).
	If emphasis is not specified, #normal will be used.
	if pixel height is not specified , then the editor font size will be used.

	This style is based on old theme and other old environments.
	"
	<styleTable: 'Vintage'>

			"(symbol color [emphasisSymbolOrArray [textStyleName [pixelHeight]]])"
 ^ #(
			(default 								black)
			(invalid 									red)

			(comment 								'006400')

			(character 								('801f91' lighter))
			(number 								('801f91'))
			(symbol 								('801f91'))
			(string 									('801f91'))


			(selector 								black)

			(#true 									('A31515' darker))
			(#false 									('A31515' darker))
			(#nil 									('A31515' darker))

			(#self 									'A31515')
			(#super								'A31515')
			(#thisContext 							'A31515')
			(#thisProcess 							'A31515')
			(#return 								'A31515' bold)

			(patternArg 							(blue muchDarker))
			(blockPatternArg 						(blue muchDarker))
			(blockArg 								(blue muchDarker))
			(argument 								(blue muchDarker))
			(blockTempVar 						(blue muchDarker))
			(blockPatternTempVar 					(blue muchDarker))

			(instVar 								(blue muchDarker))

			(tempVar 								(blue muchDarker))
			(patternTempVar 						(blue muchDarker))

			(poolConstant 							(blue muchDarker) bold)
			(classVar 								(blue muchDarker) bold)
			(globalVar 								black bold)


			(incompleteIdentifier 					blue italic)
			(incompleteSelector 					blue italic)

			(undefinedIdentifier 					red)
			(undefinedSelector 					red)

			(patternSelector 						nil bold)
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
			(cascadeSeparator 					black bold)
			(statementSeparator 					black bold)
			(methodTempBar 						black)
			(blockTempBar 						black)
			(blockArgsBar 							black)

			(pragma 						(cyan muchDarker)))
]

{ #category : 'private' }
SHRBTextStyler >> addASTTransformationPlugin: aPlugin [

	self plugins add: aPlugin
]

{ #category : 'formatting' }
SHRBTextStyler >> addAttributes: attributes forNode: aNode [

	self addAttributes: attributes from: aNode startWithoutParentheses to: aNode stopWithoutParentheses
]

{ #category : 'formatting' }
SHRBTextStyler >> addAttributes: attributes from: start to: stop [

	charAttr from: (start max: 1) to: (stop min: charAttr size) put: attributes
]

{ #category : 'converting' }
SHRBTextStyler >> addAttributesFrom: attributeRuns satisfying: aTestBlock to: aText [

	attributeRuns withStartStopAndValueDo: [:start :stop :attributes |
		attributes
			select: aTestBlock
			thenDo: [ :selectedAttribute|
				aText
					addAttribute: selectedAttribute
					from: start
					to: stop ]].

	^ aText
]

{ #category : 'accessing' }
SHRBTextStyler >> addPlugin: aPlugin [

	self plugins add: aPlugin
]

{ #category : 'formatting' }
SHRBTextStyler >> addStyle: styleSymbol attribute: additionalAttribute from: start to: end [
	self addAttributes: (self attributesFor: styleSymbol), {additionalAttribute} from: start to: end
]

{ #category : 'formatting' }
SHRBTextStyler >> addStyle: styleSymbol attributes: additionalAttributes forNode: aNode [
	self
		addAttributes: (self attributesFor: styleSymbol), additionalAttributes
		forNode: aNode
]

{ #category : 'formatting' }
SHRBTextStyler >> addStyle: styleSymbol forNode: aNode [
	self addAttributes: (self attributesFor: styleSymbol) forNode: aNode
]

{ #category : 'formatting' }
SHRBTextStyler >> addStyle: styleSymbol from: start to: end [
	self addAttributes: (self attributesFor: styleSymbol) from: start to: end
]

{ #category : 'private' }
SHRBTextStyler >> attributesFor: aSymbol [

	^ self class attributesFor: aSymbol
]

{ #category : 'accessing' }
SHRBTextStyler >> braceStyleName [

	| level |
	level := braceLevel \\ self maximumImbricationStyleSupported.
	^ (#brace , (level isZero
		    ifTrue: [ '' ]
		    ifFalse: [ level asString ])) asSymbol
]

{ #category : 'accessing' }
SHRBTextStyler >> bracketStyleName [

	| level |
	level := bracketLevel \\ self maximumImbricationStyleSupported.
	^ (#block , (level isZero
		    ifTrue: [ '' ]
		    ifFalse: [ level asString ])) asSymbol
]

{ #category : 'accessing' }
SHRBTextStyler >> classOrMetaClass: aBehavior [
	classOrMetaClass := aBehavior
]

{ #category : 'visiting' }
SHRBTextStyler >> formatIncompleteSelector: aSymbol [

	(self representsASelector: aSymbol) ifTrue: [ ^ nil ].

	^ (self class formatIncompleteIdentifiers and: [ self hasSymbolStartingBy: aSymbol ])
		  ifTrue: [ #incompleteSelector ]
		  ifFalse: [ #undefinedSelector ]
]

{ #category : 'testing' }
SHRBTextStyler >> hasSymbolStartingBy: aSymbol [

	^ (Symbol selectorThatStartsCaseSensitive: aSymbol asString skipping: nil) isNotNil
]

{ #category : 'initialization' }
SHRBTextStyler >> initialize [

	super initialize.
	stylingEnabled := true.
	isScripting := false
]

{ #category : 'accessing' }
SHRBTextStyler >> isForWorkspace [

	self
		deprecated: 'use isScripting'
		transformWith: '`@receiver isForWorkspace' -> '`@receiver isScripting'.
	^ self isScripting
]

{ #category : 'accessing' }
SHRBTextStyler >> isForWorkspace: aBoolean [
		self
		deprecated: 'use isScripting:'
		transformWith: '`@receiver isForWorkspace: `@argument' -> '`@receiver isScripting: `@argument'.
	^ self isScripting: aBoolean
]

{ #category : 'accessing' }
SHRBTextStyler >> isScripting [
	^workspace
		ifNil: [ isScripting ]
		ifNotNil: [ workspace isScripting ]
]

{ #category : 'accessing' }
SHRBTextStyler >> isScripting: aBoolean [
	isScripting := aBoolean
]

{ #category : 'private' }
SHRBTextStyler >> literalStyleSymbol: aValue [

	aValue isSymbol
		ifTrue: [ ^ #symbol ].
	aValue isString
		ifTrue: [ ^ #string ].
	aValue isCharacter
		ifTrue: [ ^ #character ].
	aValue isNumber
		ifTrue: [ ^ #number ].
	aValue == true
		ifTrue: [ ^ #true ].
	aValue == false
		ifTrue: [ ^ #false ].
	aValue ifNil: [ ^ #nil ].
	^ #default
]

{ #category : 'accessing' }
SHRBTextStyler >> maximumImbricationStyleSupported [
	"We style up to 8 level of imbricated parenthesis/brackets/braces."

	^ 8
]

{ #category : 'accessing' }
SHRBTextStyler >> parenthesisStyleName [

	| level |
	level := parentheseLevel \\ self maximumImbricationStyleSupported.
	^ (#parenthesis , (level isZero
		    ifTrue: [ '' ]
		    ifFalse: [ level asString ])) asSymbol
]

{ #category : 'private' }
SHRBTextStyler >> plugins [

	^ plugins ifNil: [ plugins := OrderedCollection new ]
]

{ #category : 'private' }
SHRBTextStyler >> privateStyle: aText [
	| compiler |

	aText ifEmpty: [ ^ self ].

	compiler := classOrMetaClass compiler
		source: aText asString;
		receiver: (self isScripting ifTrue: [workspace ifNotNil: [:w | w doItReceiver]]);
		isScripting: self isScripting;
		requestor: workspace.

	self plugins do: [ :each | compiler addParsePlugin: each ].
	
	^ self
		style: aText
		ast: compiler parse
]

{ #category : 'testing' }
SHRBTextStyler >> representsASelector: aSymbol [

	^ aSymbol isSelectorSymbol
]

{ #category : 'private' }
SHRBTextStyler >> resolveStyleFor: aVariableNode [
	^ aVariableNode styleName
]

{ #category : 'private' }
SHRBTextStyler >> resolveTextLinkFor: aVariableNode [

	aVariableNode binding isGlobalVariable
		ifTrue: [ ^ TextClassLink className: aVariableNode name ].
	^ TextVariableLink variableName: aVariableNode name
]

{ #category : 'private' }
SHRBTextStyler >> resolveVariableAttributesFor: aVariableNode [

	| textLink |
	textLink := self resolveTextLinkFor: aVariableNode.
	aVariableNode binding isGlobalVariable ifFalse: [ ^ { textLink } ].

	aVariableNode binding isDeprecated ifFalse: [ ^ { textLink } ].

	^ {
		  textLink.
		  TextEmphasis struckOut }
]

{ #category : 'styling' }
SHRBTextStyler >> style: aText [

	aText ifEmpty: [ ^ self ].
	stylingEnabled ifFalse: [ ^ self ].
	text := aText copy.
	self privateStyle: text.
	view ifNotNil: [ :view2 | view2 stylerStyled: text ]
]

{ #category : 'private' }
SHRBTextStyler >> style: aText ast: ast [

	aText ifEmpty: [ ^ self ].
	text := aText.
	charAttr := Array new: aText size withAll: (self attributesFor: #default).
	bracketLevel := 0.
	parentheseLevel := 0.
	braceLevel := 0.
	self visitNode: ast.
	"Second pass to ensure that all syntax errors are visible"
	ast allErrorNotices do: [ :notice | 
		notice isSyntaxError ifTrue: [
			self addStyle: #invalid from: notice position to: notice position ] ].
	^ aText runs: (RunArray newFrom: charAttr)
]

{ #category : 'private' }
SHRBTextStyler >> styleCloseBrace: anArrayNode [

	braceLevel := braceLevel - 1.
	(anArrayNode right isZero or: [ anArrayNode value isNil ]) ifTrue: [ ^ self ].
	self addStyle: self braceStyleName from: anArrayNode right to: anArrayNode right
]

{ #category : 'private' }
SHRBTextStyler >> styleCloseBracket: aBlockNode [
	bracketLevel := bracketLevel - 1.
	(aBlockNode right isZero or: [ aBlockNode value isNil ]) ifTrue:[ ^self ].
	self addStyle: self bracketStyleName from: aBlockNode right to: aBlockNode right
]

{ #category : 'private' }
SHRBTextStyler >> styleCloseParenthese: aMessageNode [
	aMessageNode parentheses
		ifNotEmpty: [
			aMessageNode parentheses
				do: [:interval |
					| pos |
					pos := interval last.
					parentheseLevel := parentheseLevel - 1.
					self addStyle: self parenthesisStyleName from: pos to: pos ]]
]

{ #category : 'private' }
SHRBTextStyler >> styleOpenBrace: anArrayNode [

	| style |
	style := self braceStyleName.
	self addStyle: style from: anArrayNode left to: anArrayNode left.
	braceLevel := braceLevel + 1
]

{ #category : 'private' }
SHRBTextStyler >> styleOpenBracket: aBlockNode [
	| style |
	style := self bracketStyleName.
	self addStyle: style from: aBlockNode left to: aBlockNode left.
	bracketLevel := bracketLevel + 1
]

{ #category : 'private' }
SHRBTextStyler >> styleOpenParenthese: aMessageNode [
	aMessageNode parentheses
		ifNotEmpty: [
			aMessageNode parentheses
				reverseDo: [ :interval |
					| pos |
					pos := interval first.
					self addStyle:  self parenthesisStyleName from: pos to: pos.
					parentheseLevel := parentheseLevel + 1 ]]
]

{ #category : 'visiting' }
SHRBTextStyler >> styleParenthesisOf: aNode around: aBlock [

	aNode isValue ifFalse: [ ^ aBlock value ].

	self styleOpenParenthese: aNode.
	aBlock value.
	self styleCloseParenthese: aNode
]

{ #category : 'private' }
SHRBTextStyler >> styleTempBars: aSequenceNode [
	| tempBarAttribute |
	tempBarAttribute := (aSequenceNode scope isNil or: [ aSequenceNode scope isMethodScope ])
		ifTrue: [ #methodTempBar ]
		ifFalse: [ #blockTempBar ].
	aSequenceNode leftBar
		ifNotNil: [ :pos | self addStyle: tempBarAttribute from: pos to: pos ].
	aSequenceNode rightBar
		ifNotNil: [ :pos | self addStyle: tempBarAttribute from: pos to: pos ]
]

{ #category : 'styling' }
SHRBTextStyler >> styledTextFor: aText [

	^ self privateStyle: aText
]

{ #category : 'private' }
SHRBTextStyler >> stylingEnabled [

	^ stylingEnabled
]

{ #category : 'private' }
SHRBTextStyler >> stylingEnabled: aBoolean [

	stylingEnabled := aBoolean
]

{ #category : 'styling' }
SHRBTextStyler >> unstyledTextFrom: aText [
	"return a Text object without all styling attributes"

	| answer |
	answer := Text fromString: aText string.
	self addAttributesFrom: aText runs satisfying: [ :each | each isKindOf: TextAction ] to: answer.
	^ answer
]

{ #category : 'accessing' }
SHRBTextStyler >> view: aViewOrMorph [

	view := aViewOrMorph
]

{ #category : 'visiting' }
SHRBTextStyler >> visitAnnotationMarkNode: aRBAnnotationMarkNode [

	self addStyle: #pragma from: aRBAnnotationMarkNode start to: aRBAnnotationMarkNode start.
]

{ #category : 'visiting' }
SHRBTextStyler >> visitArrayNode: anArrayNode [

	anArrayNode comments do: [ :comment | self addStyle: #comment from: comment start to: comment stop ].
	self styleOpenBrace: anArrayNode.
	anArrayNode children do: [ :each | self visitNode: each ].
	self styleCloseBrace: anArrayNode
]

{ #category : 'visiting' }
SHRBTextStyler >> visitAssignmentNode: anAssignmentNode [
	self visitNode: anAssignmentNode variable.
	self visitNode: anAssignmentNode value.
	anAssignmentNode variable binding isWritable
		ifFalse: [ self addStyle: #invalid forNode: anAssignmentNode ]
]

{ #category : 'visiting' }
SHRBTextStyler >> visitBlockNode: aBlockNode [
	aBlockNode comments
		do:
			[ :comment | self addStyle: #comment from: comment start to: comment stop ].
	aBlockNode arguments do: [ :argument | self visitNode: argument ].
	aBlockNode bar
		ifNotNil: [ :pos | self addStyle: #blockArgsBar from: pos to: pos ].
	aBlockNode colons
		do: [ :pos | self addStyle: #blockArgColon from: pos to: pos ].
	self styleOpenBracket: aBlockNode.
	self visitNode: aBlockNode body.
	self styleCloseBracket: aBlockNode
]

{ #category : 'visiting' }
SHRBTextStyler >> visitCascadeNode: aCascadeNode [
	aCascadeNode messages do: [ :each | self visitNode: each ].
	aCascadeNode semicolons do: [ :pos | self addStyle: #cascadeSeparator from: pos to: pos ]
]

{ #category : 'visiting' }
SHRBTextStyler >> visitEnglobingErrorNode: anEnglobingErrorNode [

	anEnglobingErrorNode value ifNotEmpty: [
		self addStyle: #invalid from: anEnglobingErrorNode startWithoutParentheses to: anEnglobingErrorNode startWithoutParentheses + anEnglobingErrorNode value size - 1 ].
	anEnglobingErrorNode valueAfter ifNotEmpty: [
		self addStyle: #invalid from: anEnglobingErrorNode stopWithoutParentheses to: anEnglobingErrorNode stopWithoutParentheses + anEnglobingErrorNode valueAfter size - 1 ].
	anEnglobingErrorNode contents do: [:each | self visitNode: each ]
]

{ #category : 'visiting' }
SHRBTextStyler >> visitLiteralArrayNode: aRBLiteralArrayNode [
	"We reuse the nested brackets/parenthesis style.
	The main point it to easyly match opener and closer, nor necesarily to add type semanting"

	| style |
	aRBLiteralArrayNode isForByteArray
		ifTrue: [
			style := self bracketStyleName.
			bracketLevel := bracketLevel + 1 ]
		ifFalse: [
			style := self parenthesisStyleName.
			parentheseLevel := parentheseLevel + 1 ].

	self
		addStyle: style
		from: aRBLiteralArrayNode startWithoutParentheses
		to: aRBLiteralArrayNode startWithoutParentheses + aRBLiteralArrayNode openerSize - 1.
	self
		addStyle: style
		from: aRBLiteralArrayNode stopWithoutParentheses
		to: aRBLiteralArrayNode stopWithoutParentheses.

	aRBLiteralArrayNode contents do: [ :each | self visitNode: each ].

	aRBLiteralArrayNode isForByteArray
		ifTrue: [ bracketLevel := bracketLevel - 1 ]
		ifFalse: [ parentheseLevel := parentheseLevel - 1 ]
]

{ #category : 'visiting' }
SHRBTextStyler >> visitLiteralValueNode: aLiteralValueNode [

	| value attributes |
	value := aLiteralValueNode value.
	attributes := OrderedCollection new.

	"We can have 3 different kind of links from a literal:
	- In case it is a symbol representing a class or a global, we want to return a TextClassLink to browse the class or its references. In that case we also want to check if this global is deprecated or not to strikethrought it.
	- In case it is another symbol, we want to return a TextMethodLink to browser the implementors or senders of this selector
	- In case it is another literal value, we return a TextClassLink to browse the class defining this litera."
	value isSymbol
		ifTrue: [
			"We ask the binding to be able to know if the binding is deprecated ot not. If we used #at: then we would get the value of the binding which is not what interest us."
			(self class environment bindingOf: value)
				ifNotNil: [ :aGlobalVariable |
					attributes add: (TextClassLink className: value).
					aGlobalVariable isDeprecated ifTrue: [ attributes add: TextEmphasis struckOut ] ]
				ifNil: [ attributes add: (TextMethodLink selector: value) ] ]
		ifFalse: [ TextClassLink class: value class ].
	self addStyle: (self literalStyleSymbol: value) attributes: attributes asArray forNode: aLiteralValueNode
]

{ #category : 'visiting' }
SHRBTextStyler >> visitMessageNode: aMessageNode [

	| style link |
	
	style := (self formatIncompleteSelector: aMessageNode selector) ifNil: [
		aMessageNode isAnnotation ifTrue: [ #pragma ] ifFalse: [ #selector ] ].

	link := TextMethodLink sourceNode: aMessageNode.

	aMessageNode selectorParts with: aMessageNode keywordsPositions do: [ :keyword :position |
		self
			addStyle: style
			attribute: link
			from: position
			to: position + keyword size - 1.

		(aMessageNode isCascaded not or: [ aMessageNode isFirstCascaded ]) ifTrue: [ self visitNode: aMessageNode receiver ].

		aMessageNode arguments do: [ :each | self visitNode: each ] ]
]

{ #category : 'visiting' }
SHRBTextStyler >> visitMethodNode: aMethodNode [
	| link |
	self visitMethodComments: aMethodNode.
	aMethodNode arguments do: [ :argument | self visitNode: argument ].
	link := TextMethodLink selector: aMethodNode selector.
	aMethodNode isDoIt ifFalse: [
		aMethodNode selectorParts
			with: aMethodNode keywordsPositions
			do:
				[ :keyword :position |
				self
					addStyle: #patternSelector
					attribute: link
					from: position
					to: position + keyword size - 1 ] ].
	aMethodNode pragmas do: [ :each | self visitNode: each ].
	self visitNode: aMethodNode body
]

{ #category : 'visiting' }
SHRBTextStyler >> visitNode: aNode [

	aNode comments do: [ :comment | self addStyle: #comment from: comment start to: comment stop ].

	^ self styleParenthesisOf: aNode around: [ aNode acceptVisitor: self ]
]

{ #category : 'visiting' }
SHRBTextStyler >> visitParseErrorNode: anErrorNode [

	self addStyle: #invalid forNode: anErrorNode
]

{ #category : 'visiting' }
SHRBTextStyler >> visitPragmaNode: aPragmaNode [
	"Lets imagin this pragma: `<  test: #toto test2: 2 >`

	We want to highlight the `<`, the `test:`, the `test2:` and the `>` but we do not want to highlight the arguments."

	self addStyle: #pragma from: aPragmaNode left to: aPragmaNode left.
	aPragmaNode keywordsIntervals do: [ :i | self addStyle: #pragma from: i first to: i last ].
	self addStyle: #pragma from: aPragmaNode right to: aPragmaNode right.

	aPragmaNode arguments do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
SHRBTextStyler >> visitReturnNode: aReturnNode [

	self addStyle: #return from: aReturnNode start to: aReturnNode start.

	self visitNode: aReturnNode value
]

{ #category : 'visiting' }
SHRBTextStyler >> visitSequenceNode: aSequenceNode [

	self styleTempBars: aSequenceNode.
	aSequenceNode temporaries do: [ :each | self visitNode: each ].
	aSequenceNode statements do: [ :each | self visitNode: each ].
	aSequenceNode periods do: [ :pos | self addStyle: #statementSeparator from: pos to: pos ]
]

{ #category : 'visiting' }
SHRBTextStyler >> visitVariableNode: aVariableNode [

	self addStyle: (self resolveStyleFor: aVariableNode) attributes: (self resolveVariableAttributesFor: aVariableNode) forNode: aVariableNode.
]

{ #category : 'accessing' }
SHRBTextStyler >> workspace: aWorkspace [
	workspace := aWorkspace
]
