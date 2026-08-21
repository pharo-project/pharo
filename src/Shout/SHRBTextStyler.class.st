"
A visitor for AST nodes used to add styling to the code.
Style options are organised in style tables, in methods with the pragma `<styleTable: 'aStyleName'>`. This method answer an array of arrays (a slist), where each of the content array being a description of an attribute with the form :  
```
(name  foregroundColor [text-attribute] [backgroundColor])
```
1. `name`: a symbol identifying the AST type attribute (e.g. `comment` `string`...)
2. `foregroundColor`: a foreground color for the code text. This can be an hex string (RGBA), following CSS color convention. 
	- Colors can be also specified with an array (color modifiers...), e.g. `('ff0000' lighter)`. This allows for fine tuning of the desired color.
	- This value can be `nil` meaning no color will be applied. 
3. `text attribute`: attributtes qualifying the text: `bold`, `italic`, `underlined`, `narrow`, `struckout`.
	- This value can be `nil` meaning no text emphasis will be applied.
4. `backgroundColor`. Changes background color. Same provisions than foreground color can be applied to it (but it applies to the backgound)
"
Class {
	#name : 'SHRBTextStyler',
	#superclass : 'SHTextStyler',
	#classInstVars : [
		'formatIncompleteIdentifiers',
		'styleTable',
		'styleTableSelector',
		'textAttributes'
	],
	#category : 'Shout-Styling',
	#package : 'Shout',
	#tag : 'Styling'
}

{ #category : 'attributes' }
SHRBTextStyler class >> attributeArrayForColor: colorOrNil backgroundColor: backgroundColorOrNil emphasis: emphasisSymbolOrArrayorNil [
	"answer an array of text attributes (TextAttribute) that are obtained by parsing a styleTable. 
	 We can handle foregroundColor, backgroundColor and text emphasis"
	
	^ Array streamContents: [ :stream | 
		"foreground color"
		colorOrNil ifNotNil: [ :aColor | 
			stream nextPut: (TextColor color: aColor) ].	
		"background color"
		backgroundColorOrNil ifNotNil: [ :aColor | 
			stream nextPut: (TextBackgroundColor color: aColor) ].
		"emphasis"
		emphasisSymbolOrArrayorNil ifNotNil: [
			emphasisSymbolOrArrayorNil isSymbol
				ifTrue: [ 
					stream nextPut: (TextEmphasis perform: emphasisSymbolOrArrayorNil) ]
				ifFalse: [
					emphasisSymbolOrArrayorNil do: [ :each | 
						stream nextPut: (TextEmphasis perform: each) ] ] ] ]
]

{ #category : 'attributes' }
SHRBTextStyler class >> attributeArrayForColor: aColorOrNil emphasis: anEmphasisSymbolOrArrayorNil [
	| emphArray answer |
	
	answer := Array new.
	
	aColorOrNil ifNotNil: [ answer := answer , { (TextColor color: aColorOrNil) } ].
	anEmphasisSymbolOrArrayorNil ifNotNil: [
		emphArray := anEmphasisSymbolOrArrayorNil isSymbol
			ifTrue: [ { anEmphasisSymbolOrArrayorNil } ]
			ifFalse: [ anEmphasisSymbolOrArrayorNil ].
		emphArray 
			do: [ :each | each ~= #normal ifTrue: [ answer := answer , { (TextEmphasis perform: each) } ] ] ].

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
			('default' 								black)
			('invalid' 									red)

			('comment' 								(gray darker))

			('character' 								(red muchDarker))
			('number' 								(red muchDarker))
			('symbol' 								(magenta muchDarker))
			('string' 									(magenta muchDarker) normal)

			('selector' 								(black))

			('self' 									(cyan muchDarker ))
			('super' 								(cyan muchDarker ))
			('true' 									(red muchDarker))
			('false' 									(red muchDarker))
			('nil' 									(red muchDarker))
			('thisContext' 							(cyan muchDarker ))
			('thisProcess' 							(cyan muchDarker ))
			('return' 								(cyan muchDarker ) bold)
			('patternArg' 							(blue muchDarker))
			('blockPatternArg' 						(blue muchDarker))
			('blockArg' 								(blue muchDarker))
			('argument' 								(blue muchDarker))
			('blockTempVar' 						(blue muchDarker))
			('blockPatternTempVar' 					(blue muchDarker))
			('instVar' 								(cyan muchDarker))

			('tempVar' 								(blue muchDarker))
			('patternTempVar' 						(blue muchDarker))
			('poolConstant' 							(cyan muchDarker))
			('classVar' 								(cyan muchDarker))
			('globalVar' 								(purple muchDarker))

			('incompleteIdentifier' 					blue italic)
			('incompleteSelector' 					nil  italic)

			('undefinedIdentifier' 					red)
			('undefinedSelector' 					red)

			('patternSelector' 						nil bold)
			('blockArgColon' 						black)
			('parenthesis' 						black)
			('parenthesis1' 						(green muchDarker))
			('parenthesis2' 						(magenta muchDarker))
			('parenthesis3' 						(red muchDarker))
			('parenthesis4' 						(green darker))
			('parenthesis5' 						(orange darker))
			('parenthesis6' 						(magenta darker))
			('parenthesis7' 						blue)
			('block' 								black)
			('block1' 							(green muchDarker))
			('block2' 							(magenta muchDarker))
			('block3' 							(red muchDarker))
			('block4' 							(green darker))
			('block5' 							(orange darker))
			('block6' 							(magenta darker))
			('block7' 							blue)
			('brace' 								black)
			('brace1' 								(green muchDarker))
			('brace2' 								(magenta muchDarker))
			('brace3' 								(red muchDarker))
			('brace4' 								(green darker))
			('brace5' 								(orange darker))
			('brace6' 								(magenta darker))
			('brace7' 								blue)
			('cascadeSeparator' 					black)
			('statementSeparator' 					black)
			('methodTempBar' 						(black))
			('blockTempBar' 						(black))
			('blockArgsBar' 							(black))

			('pragma' 						(green muchDarker)))
]

{ #category : 'styles' }
SHRBTextStyler class >> darkBlueStyleTable [
	<styleTable: 'DarkBlue'>

			"(symbol color [emphasisSymbolOrArray [textStyleName [pixelHeight]]])"
 	^ #(
			('default' 								white)
			('invalid' 								'FF8A80') 

			('comment' 								'7D8C93')

			('character' 							('E38C1E' lighter))
			('number' 								('FFE54B'))
			('symbol' 								('E38C1E'))
			('string' 								('E38C1E'))

			('selector' 								white)

			('true' 								('00CC6E' darker))
			('false' 								('00CC6E' darker))
			('nil' 									('00CC6E' darker))

			('self' 								'00CC6E')
			('super'								'00CC6E')
			('thisContext' 						'00CC6E')
			('return' 								'00CC6E' bold)

			('patternArg' 							'03a9f4' bold)
			('argument' 							'03a9f4')
										
			('instVar' 								'ffa000')										

			('patternTempVar' 					'00acc1')
			('tempVar' 								'00acc1')
																					
			('blockPatternArg' 					'0091ea' italic)
			('blockArg' 							'0091ea' italic)

			('blockPatternTempVar' 				'00838f' italic)
			('blockTempVar' 						'00838f' italic)

			('poolConstant' 						'A7E2ED' bold)
			('classVar' 							'A7E2ED' bold)
			('globalVar' 							white bold)


			('incompleteIdentifier' 			'E8E2B7' italic)
			('incompleteSelector'				'E8E2B7' italic)

			('undefinedIdentifier' 				'FF8A80')
			('undefinedSelector' 					'FF8A80')

			('patternSelector' 						nil bold)
			('blockArgColon' 						white)
			('parenthesis' 					white)
			('parenthesis1' 					(green muchLighter))
			('parenthesis2' 					(magenta muchLighter))
			('parenthesis3' 					('FF8A80' muchLighter))
			('parenthesis4' 					(green lighter))
			('parenthesis5' 					(orange lighter))
			('parenthesis6' 					(magenta lighter))
			('parenthesis7' 					blue)
			('block' 							white)
			('block1' 						(green muchLighter))
			('block2' 						(magenta muchLighter))
			('block3' 						(red muchLighter))
			('block4' 						(green lighter))
			('block5' 						(orange lighter))
			('block6' 						(magenta lighter))
			('block7' 						blue)
			('brace' 							white)
			('cascadeSeparator' 					white bold)
			('statementSeparator' 				white bold)
			('methodTempBar' 						white)
			('blockTempBar' 						white)
			('blockArgsBar' 						white)

			
			('pragma' 									'B4DD6E'))
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
			('default' 								white)
			('invalid' 								'FF8A80')

			('comment' 								'7D8C93')

			('character' 							('E38C1E' lighter))
			('number' 								('FFE54B'))
			('symbol' 								('E38C1E'))
			('string' 								('E38C1E'))

			('selector' 								white)

			('true' 								('00CC6E' darker))
			('false' 								('00CC6E' darker))
			('nil' 									('00CC6E' darker))

			('self' 								'00CC6E')
			('super'								'00CC6E')
			('thisContext' 						'00CC6E')
			('thisProcess' 						'00CC6E')
			('return' 								'00CC6E' bold)

			('patternArg' 							'A7E2ED')
			('blockPatternArg' 					'A7E2ED' italic)
			('blockArg' 							'A7E2ED' italic)
			('argument' 							'A7E2ED')
			('blockTempVar' 						'A7E2ED')
			('blockPatternTempVar' 				'A7E2ED')

			('instVar' 								'A7E2ED')

			('tempVar' 								'A7E2ED')
			('patternTempVar' 					'A7E2ED')

			('poolConstant' 						'A7E2ED' bold)
			('classVar' 							'A7E2ED' bold)
			('globalVar' 							white bold)


			('incompleteIdentifier' 			'E8E2B7' italic)
			('incompleteSelector' 				'E8E2B7' italic)

			('undefinedIdentifier' 				'FF8A80')
			('undefinedSelector' 					'FF8A80')

			('patternSelector' 					nil bold)
			('blockArgColon' 						white)
			('parenthesis' 					white)
			('parenthesis1' 					(green muchLighter))
			('parenthesis2' 					(magenta muchLighter))
			('parenthesis3' 					('FF8A80' muchLighter))
			('parenthesis4' 					(green lighter))
			('parenthesis5' 					(orange lighter))
			('parenthesis6' 					(magenta lighter))
			('parenthesis7' 					blue)
			('block' 							white)
			('block1' 						(green muchLighter))
			('block2' 						(magenta muchLighter))
			('block3' 						(red muchLighter))
			('block4' 						(green lighter))
			('block5' 						(orange lighter))
			('block6' 						(magenta lighter))
			('block7' 						blue)
			('brace' 							white)
			('brace1' 						(green muchLighter))
			('brace2' 						(magenta muchLighter))
			('brace3' 						(red muchLighter))
			('brace4' 						(green lighter))
			('brace5' 						(orange lighter))
			('brace6' 						(magenta lighter))
			('brace7' 						blue)
			('cascadeSeparator' 					white bold)
			('statementSeparator' 				white bold)
			('methodTempBar' 						white)
			('blockTempBar' 						white)
			('blockArgsBar' 						white)

			('pragma' 									'B4DD6E'))
]

{ #category : 'styles' }
SHRBTextStyler class >> defaultStyleTableSelector [
	"Answer the selector identifying the default style table"

 	^ #blueStyleTable
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
	"Parses aStyleTable and answer a dictionary of attributes, with attribute name as key."

	| dictionary |
	dictionary := Dictionary new.
	aStyleTable do: [ :each |
			| element emphasis color backgroundColor |
			element := each at: 1.
			color := each at: 2 ifAbsent: [ ].
			color := color ifNotNil: [ Color colorFrom: color ].
			emphasis := each at: 3 ifAbsent: [ ].
			backgroundColor := each at: 4 ifAbsent: [ ].
			backgroundColor := backgroundColor ifNotNil: [ Color colorFrom: backgroundColor ].

			(self attributeArrayForColor: color backgroundColor: backgroundColor emphasis: emphasis) ifNotEmpty: [ :attrArray |
				dictionary at: element put: attrArray ] ].

	^ dictionary
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
SHRBTextStyler class >> reset [
	<script>

	styleTable := nil.
	styleTableSelector := nil.
	textAttributes := nil
]

{ #category : 'initialization' }
SHRBTextStyler class >> resetTextAttributesCaches [

	<script>
	textAttributes := nil
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
			('default' 								('657A81' muchDarker))
			('invalid' 									red)

			('comment' 								'586E75')

			('character' 								('268BD2' lighter))
			('number' 								('2AA198'))
			('symbol' 								('268BD2'))
			('string' 									('268BD2'))


			('selector' 								nil)

			('true' 									('CB4B16' darker))
			('false' 									('CB4B16' darker))
			('nil' 									('CB4B16' darker))

			('self' 									'B58900')
			('super'								'B58900')
			('thisContext' 							'B58900')
			('thisProcess' 							'B58900')
			('return' 								'B58900' bold)

			('patternArg' 							('D33682'))
			('blockPatternArg' 						('D33682'))
			('blockArg' 								('D33682'))
			('argument' 								('D33682'))
			('blockTempVar' 						('D33682'))
			('blockPatternTempVar' 					('D33682'))

			('instVar' 								('D33682'))

			('tempVar' 								('D33682'))
			('patternTempVar' 						('D33682'))

			('poolConstant' 							('D33682') bold)
			('classVar' 								('D33682') bold)
			('globalVar' 								('268BD2' muchDarker) bold)

			('undefinedIdentifier' 					red)
			('incompleteIdentifier' 					'D33682' italic)
			('incompleteSelector' 					nil  italic)
			('undefinedSelector' 					red)
			('patternSelector' 						'657A81' bold)
			('blockArgColon' 						black)
			('parenthesis' 						black)
			('parenthesis1' 						(green muchDarker))
			('parenthesis2' 						(magenta muchDarker))
			('parenthesis3' 						(red muchDarker))
			('parenthesis4' 						(green darker))
			('parenthesis5' 						(orange darker))
			('parenthesis6' 						(magenta darker))
			('parenthesis7' 						blue)
			('block' 								black)
			('block1' 							(green muchDarker))
			('block2' 							(magenta muchDarker))
			('block3' 							(red muchDarker))
			('block4' 							(green darker))
			('block5' 							(orange darker))
			('block6' 							(magenta darker))
			('block7' 							blue)
			('brace' 								black)
			('brace1' 								(green muchDarker))
			('brace2' 								(magenta muchDarker))
			('brace3' 								(red muchDarker))
			('brace4' 								(green darker))
			('brace5' 								(orange darker))
			('brace6' 								(magenta darker))
			('brace7' 								blue)
			('cascadeSeparator' 					black bold)
			('statementSeparator' 					black bold)
			('methodTempBar' 						black)
			('blockTempBar' 						black)
			('blockArgsBar' 							black)

			('pragma' 						(cyan muchDarker)))
]

{ #category : 'accessing' }
SHRBTextStyler class >> styleTable [

	^ styleTable ifNil: [ 
		styleTable := self perform: self styleTableSelector ]
]

{ #category : 'accessing' }
SHRBTextStyler class >> styleTable: anArray [

	styleTable := anArray.
	self resetTextAttributesCaches
]

{ #category : 'styles' }
SHRBTextStyler class >> styleTableName [
	"Answer a <String> representing the current style name"
	
	self styleTablePragmas 
		detect: [ : p | (self perform: p methodSelector) = self styleTable ]
		ifFound: [ : p | ^ p argumentAt: 1 ]
		ifNone: [ self error: 'Style table unknown' ]

]

{ #category : 'styles' }
SHRBTextStyler class >> styleTableNamed: aString [

	^ self styleTablePragmas
		detect: [ :each | (each argumentAt: 1) = aString ]
		ifFound: [ :each | self perform: each methodSelector ]
		ifNone: [ nil ]
]

{ #category : 'styles' }
SHRBTextStyler class >> styleTablePragmas [
	"Answer a <Collection> of <Pragma> each representing a Shout style"

	^ Pragma
		  allNamed: #styleTable:
		  from: self class
		  to: SHRBTextStyler class
]

{ #category : 'accessing' }
SHRBTextStyler class >> styleTableSelector [

	^ styleTableSelector ifNil: [ styleTableSelector := self defaultStyleTableSelector ]
]

{ #category : 'accessing' }
SHRBTextStyler class >> styleTableSelector: aSelector [

	styleTableSelector := aSelector.
	self styleTable: (self perform: styleTableSelector)
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
			('default' 								black)
			('invalid' 									red)

			('comment' 								'888a85')

			('character' 								'ce5c00')
			('number' 								'8f5902')
			('symbol' 								'c4a000')
			('string' 									'ce5c00')


			('selector' 								(black))

			('true' 									'8F5902')
			('false' 									'8F5902')
			('nil' 									'8F5902')

			('self' 									'4d9a06')
			('super' 								'4d9a06')
			('thisContext' 							'4d9a06')
			('thisProcess' 							'4d9a06')
			('return' 								black bold)

			('patternArg' 							'2e3426' italic)
			('blockPatternArg' 						'888a85' bold)
			('blockArg' 								'555753' italic)
			('argument' 								'555753' italic)
			('blockTempVar' 						'555753' italic)
			('blockPatternTempVar' 					'888a85' italic)

			('instVar' 								'204a87')

			('tempVar' 								'555753' italic)
			('patternTempVar' 						'888a85' italic)

			('poolConstant' 							'ad7fa8' bold)
			('classVar' 								'ad7fa8' bold)
			('globalVar' 								'5c3566' bold)


			('incompleteIdentifier' 					blue italic)
			('incompleteSelector' 					blue italic)

			('undefinedIdentifier' 					red)
			('undefinedSelector' 					red)
			('patternSelector' 						nil bold)
			('blockArgColon' 						black)
			('parenthesis' 						black)
			('parenthesis1' 						(green muchDarker))
			('parenthesis2' 						(magenta muchDarker))
			('parenthesis3' 						(red muchDarker))
			('parenthesis4' 						(green darker))
			('parenthesis5' 						(orange darker))
			('parenthesis6' 						(magenta darker))
			('parenthesis7' 						blue)
			('block' 								black)
			('block1' 							(green muchDarker))
			('block2' 							(magenta muchDarker))
			('block3' 							(red muchDarker))
			('block4' 							(green darker))
			('block5' 							(orange darker))
			('block6' 							(magenta darker))
			('block7' 							blue)
			('brace' 								black)
			('brace1' 								(green muchDarker))
			('brace2' 								(magenta muchDarker))
			('brace3' 								(red muchDarker))
			('brace4' 								(green darker))
			('brace5' 								(orange darker))
			('brace6' 								(magenta darker))
			('brace7' 								blue)
			('cascadeSeparator' 					black)
			('statementSeparator' 					black)
			('methodTempBar' 						black)
			('blockTempBar' 						black)
			('blockArgsBar' 							black)

			('pragma' 						(green muchDarker)))
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
			('default' 								black)
			('invalid' 									red)

			('comment' 								'006400')

			('character' 								('801f91' lighter))
			('number' 								('801f91'))
			('symbol' 								('801f91'))
			('string' 									('801f91'))


			('selector' 								black)

			('true' 									('A31515' darker))
			('false' 									('A31515' darker))
			('nil' 									('A31515' darker))

			('self' 									'A31515')
			('super'								'A31515')
			('thisContext' 							'A31515')
			('thisProcess' 							'A31515')
			('return' 								'A31515' bold)

			('patternArg' 							(blue muchDarker))
			('blockPatternArg' 						(blue muchDarker))
			('blockArg' 								(blue muchDarker))
			('argument' 								(blue muchDarker))
			('blockTempVar' 						(blue muchDarker))
			('blockPatternTempVar' 					(blue muchDarker))

			('instVar' 								(blue muchDarker))

			('tempVar' 								(blue muchDarker))
			('patternTempVar' 						(blue muchDarker))

			('poolConstant' 							(blue muchDarker) bold)
			('classVar' 								(blue muchDarker) bold)
			('globalVar' 								black bold)


			('incompleteIdentifier' 					blue italic)
			('incompleteSelector' 					blue italic)

			('undefinedIdentifier' 					red)
			('undefinedSelector' 					red)

			('patternSelector' 						nil bold)
			('blockArgColon' 						black)
			('parenthesis' 						black)
			('parenthesis1' 						(green muchDarker))
			('parenthesis2' 						(magenta muchDarker))
			('parenthesis3' 						(red muchDarker))
			('parenthesis4' 						(green darker))
			('parenthesis5' 						(orange darker))
			('parenthesis6' 						(magenta darker))
			('parenthesis7' 						blue)
			('block' 								black)
			('block1' 							(green muchDarker))
			('block2' 							(magenta muchDarker))
			('block3' 							(red muchDarker))
			('block4' 							(green darker))
			('block5' 							(orange darker))
			('block6' 							(magenta darker))
			('block7' 							blue)
			('brace' 								black)
			('brace1' 								(green muchDarker))
			('brace2' 								(magenta muchDarker))
			('brace3' 								(red muchDarker))
			('brace4' 								(green darker))
			('brace5' 								(orange darker))
			('brace6' 								(magenta darker))
			('brace7' 								blue)
			('cascadeSeparator' 					black bold)
			('statementSeparator' 					black bold)
			('methodTempBar' 						black)
			('blockTempBar' 						black)
			('blockArgsBar' 							black)

			('pragma' 						(cyan muchDarker)))
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

{ #category : 'private' }
SHRBTextStyler >> style: aText ast: ast [

	aText ifEmpty: [ ^ self ].
	charAttr := Array new: aText size withAll: (self attributesFor: #default).
	bracketLevel := 0.
	parentheseLevel := 0.
	braceLevel := 0.
	self visitNode: ast.
	"Second pass to ensure that all syntax errors are visible"
	ast allErrorNotices do: [ :notice | notice isSyntaxError ifTrue: [ self addStyle: #invalid from: notice position to: notice position ] ].
	^ aText runs: (RunArray newFrom: charAttr)
]

{ #category : 'styling' }
SHRBTextStyler >> unstyledTextFrom: aText [
	"return a Text object without all styling attributes"

	| answer |
	answer := Text fromString: aText string.
	self addAttributesFrom: aText runs satisfying: [ :each | each isKindOf: TextAction ] to: answer.
	^ answer
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
		aMessageNode isAnnotation ifTrue: [ 'pragma' ] ifFalse: [ 'selector' ] ].


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
