"
I style Smalltalk methods and expressions.

My 'styleTable' class instance var holds an array ofArrays which control how each token is styled/coloured. See my defaultStyleTable class method for its structure.
My styleTable can be changed by either modifying the defaultStyleTable class method and then executing SHTextStylerST80 initialize ; or by giving me a new styleTable through my #styleTable: class method.

My 'textAttributesByPixelSize' class instance var contains a dictionary of dictionaries.
	The key is a pixelSize and the value a Dictionary from token type Symbol to TextAttribute array.
	It is created/maintained automatically.

I reimplement #unstyledTextFrom: so that TextActions are preserved in the unstyled text 
	
	
	
	
	 
	

"
Class {
	#name : #SHTextStylerST80,
	#superclass : #SHTextStyler,
	#instVars : [
		'classOrMetaClass',
		'workspace',
		'isForWorkspace',
		'font',
		'parser',
		'environment',
		'pixelHeight'
	],
	#classInstVars : [
		'styleTable',
		'textAttributesByPixelHeight'
	],
	#category : 'Shout-Styling'
}

{ #category : #attributes }
SHTextStylerST80 class >> attributeArrayForColor: aColorOrNil emphasis: anEmphasisSymbolOrArrayorNil font: aTextStyleOrFontOrNil [ 

	| emphArray answer |	
	answer := Array new.
	aColorOrNil ifNotNil: [ answer := answer , {TextColor color: aColorOrNil}].
	
	anEmphasisSymbolOrArrayorNil
		ifNotNil: [
			emphArray := anEmphasisSymbolOrArrayorNil isSymbol
						ifTrue: [{anEmphasisSymbolOrArrayorNil}]
						ifFalse: [anEmphasisSymbolOrArrayorNil].
			emphArray do: [:each | 
				each ~= #normal
					ifTrue: [answer := answer , {TextEmphasis perform: each}]]].
	
	aTextStyleOrFontOrNil
		ifNotNil: [ answer := answer , {TextFontReference toFont: aTextStyleOrFontOrNil}].
		
	^ answer
]

{ #category : #attributes }
SHTextStylerST80 class >> attributesFor: aSymbol pixelHeight: aNumber [ 
	^ (self textAttributesByPixelHeight
		at: aNumber
		ifAbsentPut: [self initialTextAttributesForPixelHeight: aNumber])
		at: aSymbol
		ifAbsent: [#()]
]

{ #category : #styles }
SHTextStylerST80 class >> blueStyleTable [
	"color can be a valid argument to Color class>>colorFrom: , or nil to
	use the editor text color.
	Multiple emphases can be specified using an array e.g. #(bold italic).
	If emphasis is not specified, #normal will be used.
	if pixel height is not specified , then the editor font size will be used.
	"
	<styleTable: 'Blue'>
								
 ^ #(
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
			(blockTempVar 						(blue muchDarker))
			(blockPatternTempVar 					(blue muchDarker))
			(instVar 								(blue muchDarker))
			(workspaceVar 							(blue muchDarker))
		
			(tempVar 								(blue muchDarker))
			(patternTempVar 						(blue muchDarker))
			(poolConstant 							(blue muchDarker))
			(classVar 								(blue muchDarker))
			(globalVar 								(blue muchDarker))
												
			(incompleteIdentifier 					blue italic)
			(incompleteKeyword 					nil  italic)
			(incompleteBinary 						nil  italic) 
			(incompleteUnary 						nil  italic)					
				
			(undefinedIdentifier 					red)						
			(undefinedKeyword 					red)
			(undefinedBinary 						red) 
			(undefinedUnary 						red) 
	
			(patternKeyword 						nil bold)
			(patternBinary 							nil bold)
			(patternUnary 							nil bold) 
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
			(methodTempBar 						(black))
			(blockTempBar 						(black))
			(blockArgsBar 							(black))
										
			(externalCallType 						black)
			(externalCallTypePointerIndicator 		black)
			(primitiveOrExternalCallStart 			black bold)
			(primitiveOrExternalCallEnd 			black bold)
			(primitive 								(green muchDarker))
			(pragmaKeyword 						(green muchDarker))
			(pragmaUnary 							(green muchDarker))
			(pragmaBinary 							(green muchDarker)) 
			(externalFunctionCallingConvention 	(green muchDarker) bold)
			(module 								(green muchDarker) bold))
]

{ #category : #styles }
SHTextStylerST80 class >> darkStyleTable [
	"color can be a valid argument to Color class>>colorFrom: , or nil to
	use the editor text color.
	Multiple emphases can be specified using an array e.g. #(bold italic).
	If emphasis is not specified, #normal will be used.
	if pixel height is not specified , then the editor font size will be used.
	
	This style is inspired on darkula theme from eclipse: http://eclipsecolorthemes.org/?view=theme&id=15515.  
	"
	<styleTable: 'Dark'>

 ^ #(
			"(symbol color [emphasisSymbolOrArray [textStyleName [pixelHeight]]])" 
			(default 								white)
			(invalid 								'FF8A80')
			(excessCode 							'FF8A80')
			
			(comment 								'7D8C93')
			(unfinishedComment 				'FF8A80')
			
			(#'$' 									('E38C1E' lighter))
			(character 							('E38C1E' lighter))
			(integer 								('FFE54B'))
			(number 								('FFE54B'))
			(#- 									('FFE54B'))
			(symbol 								('E38C1E')) 
			(stringSymbol 						('E38C1E')) 
			(literalArray 						white)
			(string 								('E38C1E'))
												
			(unfinishedString 					'FF8A80' normal)
			(assignment 							nil bold)
			(ansiAssignment 					nil bold)
										
			(literal 								nil italic)
												
			(keyword 								white)
			(binary 								white) 
			(unary 								white)
										
			(#true 								('00CC6E' darker))
			(#false 								('00CC6E' darker))
			(#nil 									('00CC6E' darker))
													
			(#self 								'00CC6E')
			(#super								'00CC6E')
			(#thisContext 						'00CC6E')
			(#return 								'00CC6E' bold)
												
			(patternArg 							'A7E2ED') 
			(methodArg 							'A7E2ED')
			(blockPatternArg 					'A7E2ED' italic)
			(blockArg 							'A7E2ED' italic)
			(argument 							'A7E2ED')
			(blockTempVar 						'A7E2ED')
			(blockPatternTempVar 				'A7E2ED')
								
			(instVar 								'A7E2ED')
			(workspaceVar 						'A7E2ED' bold)
										
			(tempVar 								'A7E2ED')
			(patternTempVar 					'A7E2ED')
										
			(poolConstant 						'A7E2ED' bold)
			(classVar 							'A7E2ED' bold)
			(globalVar 							white bold)
												
		
			(incompleteIdentifier 			'E8E2B7' italic)
			(incompleteKeyword 				'E8E2B7' italic)
			(incompleteBinary 					'E8E2B7' italic) 
			(incompleteUnary 					'E8E2B7' italic)
				
			(undefinedIdentifier 				'FF8A80')						
			(undefinedKeyword 					'FF8A80')
			(undefinedBinary 					'FF8A80') 
			(undefinedUnary 					'FF8A80') 
			
			(patternKeyword 					nil bold)
			(patternBinary 						nil bold)
			(patternUnary 						nil bold) 
			(blockArgColon 						white)
			(leftParenthesis 					white)
			(rightParenthesis 					white)
			(leftParenthesis1 					(green muchLighter))
			(rightParenthesis1 				(green muchLighter))
			(leftParenthesis2 					(magenta muchLighter))
			(rightParenthesis2 				(magenta muchLighter))
			(leftParenthesis3 					('FF8A80' muchLighter))
			(rightParenthesis3 				('FF8A80' muchLighter))
			(leftParenthesis4 					(green lighter))
			(rightParenthesis4 				(green lighter))
			(leftParenthesis5 					(orange lighter))
			(rightParenthesis5 				(orange lighter))
			(leftParenthesis6 					(magenta lighter))
			(rightParenthesis6 				(magenta lighter))
			(leftParenthesis7 					blue)
			(rightParenthesis7 				blue)
			(blockStart 							white)
			(blockEnd 							white)
			(blockStart1 						(green muchLighter))
			(blockEnd1 							(green muchLighter))
			(blockStart2 						(magenta muchLighter))
			(blockEnd2 							(magenta muchLighter))
			(blockStart3 						(red muchLighter))
			(blockEnd3 							(red muchLighter))
			(blockStart4 						(green lighter))
			(blockEnd4 							(green lighter))
			(blockStart5 						(orange lighter))
			(blockEnd5 							(orange lighter))
			(blockStart6 						(magenta lighter))
			(blockEnd6 							(magenta lighter))
			(blockStart7 						blue)
			(blockEnd7 							blue) 
			(arrayStart 							white)
			(arrayEnd 							white)
			(arrayStart1 						white)
			(arrayEnd1 							white)
			(leftBrace 							white)
			(rightBrace 							white)
			(cascadeSeparator 					white bold)
			(statementSeparator 				white bold)
			(methodTempBar 						white)
			(blockTempBar 						white)
			(blockArgsBar 						white)
										
			(externalCallType 								white)
			(externalCallTypePointerIndicator 		white)
			(primitiveOrExternalCallStart 				white bold)
			(primitiveOrExternalCallEnd 				white bold)
			(primitive 										'B4DD6E')
			(pragmaKeyword 									'B4DD6E')
			(pragmaUnary 									'B4DD6E')
			(pragmaBinary 									'B4DD6E') 
			(externalFunctionCallingConvention 		'B4DD6E' bold)
			(module 											'B4DD6E' bold))
]

{ #category : #styles }
SHTextStylerST80 class >> defaultStyleTable [
	"color can be a valid argument to Color class>>colorFrom: , or nil to
	use the editor text color.
	Multiple emphases can be specified using an array e.g. #(bold italic).
	If emphasis is not specified, #normal will be used.
	if pixel height is not specified , then the editor font size will be used.
	"
								
 	^ self blueStyleTable
]

{ #category : #attributes }
SHTextStylerST80 class >> initialTextAttributesForPixelHeight: aNumber [ 
	| d |
	d := IdentityDictionary new.
	self styleTable
		do: [:each | 
			| textStyle element emphasis font pixelHeight attrArray color textStyleName |
			element := each first.
			color := each
						at: 2
						ifAbsent: [].
			color := color
						ifNotNil: [Color colorFrom: color].
			emphasis := each
						at: 3
						ifAbsent: [].
			textStyleName := each
						at: 4
						ifAbsent: [].
			pixelHeight := each
						at: 5
						ifAbsent: [aNumber].
			textStyleName
				ifNil: [pixelHeight := nil].
			textStyle := TextStyle named: textStyleName.
			font := textStyle
						ifNotNil: [pixelHeight
								ifNotNil: [textStyle fontOfSize: pixelHeight]].
			attrArray := self
						attributeArrayForColor: color
						emphasis: emphasis
						font: font.
			attrArray notEmpty
				ifTrue: [d at: element put: attrArray]].
	^ d
]

{ #category : #initialization }
SHTextStylerST80 class >> initialize [
	"self initialize"
	styleTable := nil.
	textAttributesByPixelHeight := nil.
]

{ #category : #styles }
SHTextStylerST80 class >> solarizedStyleTable [
	"color can be a valid argument to Color class>>colorFrom: , or nil to
	use the editor text color.
	Multiple emphases can be specified using an array e.g. #(bold italic).
	If emphasis is not specified, #normal will be used.
	if pixel height is not specified , then the editor font size will be used.
	
	This style is based on old theme and other old environments.  
	"
	<styleTable: 'Solarized'>
								
 ^ #(
			"(symbol color [emphasisSymbolOrArray [textStyleName [pixelHeight]]])" 
			(default 								('657A81' muchDarker))
			(invalid 									red)
			(excessCode 							red)
			
			(comment 								'586E75')
			(unfinishedComment 					(red muchDarker))
			
			(#'$' 									('268BD2' lighter))
			(character 								('268BD2' lighter))
			(integer 								('2AA198'))
			(number 								('2AA198'))
			(#- 										('2AA198'))
			(symbol 								('268BD2')) 
			(stringSymbol 							('268BD2')) 
			(literalArray 							black)
			(string 									('268BD2'))
												
			(unfinishedString 						red normal)
			(assignment 							nil bold)
			(ansiAssignment 						nil bold)
										
			(literal 									nil italic)
												
			(keyword 								nil)
			(binary 									nil) 
			(unary 									nil)
										
			(#true 									('CB4B16' darker))
			(#false 									('CB4B16' darker))
			(#nil 									('CB4B16' darker))
													
			(#self 									'B58900')
			(#super								'B58900')
			(#thisContext 							'B58900')
			(#return 								'B58900' bold)
												
			(patternArg 							('D33682')) 
			(methodArg 							('D33682'))
			(blockPatternArg 						('D33682'))
			(blockArg 								('D33682'))
			(argument 								('D33682'))
			(blockTempVar 						('D33682'))
			(blockPatternTempVar 					('D33682'))
								
			(instVar 								('D33682'))
			(workspaceVar 							('D33682') bold)
										
			(tempVar 								('D33682'))
			(patternTempVar 						('D33682'))
										
			(poolConstant 							('D33682') bold)
			(classVar 								('D33682') bold)
			(globalVar 								('268BD2' muchDarker) bold)
												
			(undefinedIdentifier 					red)
			(incompleteIdentifier 					'D33682' italic)
			(incompleteKeyword 					nil  italic)
			(incompleteBinary 						nil  italic) 
			(incompleteUnary 						nil  italic)
			(undefinedKeyword 					red)
			(undefinedBinary 						red) 
			(undefinedUnary 						red) 
			(patternKeyword 						'657A81' bold)
			(patternBinary 							'657A81' bold)
			(patternUnary 							'657A81' bold) 
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
			(cascadeSeparator 					black bold)
			(statementSeparator 					black bold)
			(methodTempBar 						black)
			(blockTempBar 						black)
			(blockArgsBar 							black)
										
			(externalCallType 						black)
			(externalCallTypePointerIndicator 		black)
			(primitiveOrExternalCallStart 			black bold)
			(primitiveOrExternalCallEnd 			black bold)
			(primitive 								(cyan muchDarker))
			(pragmaKeyword 						(cyan muchDarker))
			(pragmaUnary 							(cyan muchDarker))
			(pragmaBinary 							(cyan muchDarker)) 
			(externalFunctionCallingConvention 	(cyan muchDarker) bold)
			(module 								(cyan muchDarker) bold))
]

{ #category : #accessing }
SHTextStylerST80 class >> styleTable [
	^ styleTable
		ifNil: [styleTable := self defaultStyleTable]
]

{ #category : #accessing }
SHTextStylerST80 class >> styleTable: anArray [ 
	styleTable := anArray.
	textAttributesByPixelHeight := nil
]

{ #category : #styles }
SHTextStylerST80 class >> tangoStyleTable [
	"color can be a valid argument to Color class>>colorFrom: , or nil to
	use the editor text color.
	Multiple emphases can be specified using an array e.g. #(bold italic).
	If emphasis is not specified, #normal will be used.
	if pixel height is not specified , then the editor font size will be used.
	
	This is a syntax coloring scheme based on the tango desktop icon scheme:
	http://tango.freedesktop.org/Tango_Icon_Theme_Guidelines
	"
	<styleTable: 'Tango'>
								
 ^ #(
			"(symbol color [emphasisSymbolOrArray [textStyleName [pixelHeight]]])" 
			(default 								black)
			(invalid 									red)
			(excessCode 							red)
			
			(comment 								'888a85')
			(unfinishedComment 					(red muchDarker))
			
			(#'$' 									'ce5c00')
			(character 								'ce5c00')
			(integer 								'8f5902')
			(number 								'8f5902')
			(#- 										'8f5902')
			(symbol 								'c4a000') 
			(stringSymbol 							'c4a000') 
			(literalArray 							black)
			(string 									'ce5c00')
												
			(unfinishedString 						red normal)
			(assignment 							nil)
			(ansiAssignment 						nil)
										
			(literal 									'8F5902')
												
			(keyword 								(black))
			(binary 									(black)) 
			(unary 									(black))
										
			(#true 									'8F5902')
			(#false 									'8F5902')
			(#nil 									'8F5902')
													
			(#self 									'4d9a06')
			(#super 								'4d9a06')
			(#thisContext 							'4d9a06')
			(#return 								black bold)
												
			(patternArg 							'2e3426' italic) 
			(methodArg 							'555753' italic)
			(blockPatternArg 						'888a85' bold)
			(blockArg 								'555753' italic)
			(argument 								'555753' italic)
			(blockTempVar 						'555753' italic)
			(blockPatternTempVar 					'888a85' italic)
								
			(instVar 								'204a87')
			(workspaceVar 							'204a87')
										
			(tempVar 								'555753' italic)
			(patternTempVar 						'888a85' italic)
										
			(poolConstant 							'ad7fa8' bold)
			(classVar 								'ad7fa8' bold)
			(globalVar 								'5c3566' bold)
												
			
			(incompleteIdentifier 					blue italic)
			(incompleteKeyword 					blue italic)
			(incompleteBinary 						blue italic) 
			(incompleteUnary 						blue italic)
									
			(undefinedIdentifier 					red)						
			(undefinedKeyword 					red)
			(undefinedBinary 						red) 
			(undefinedUnary 						red) 
			(patternKeyword 						nil bold)
			(patternBinary 							nil bold)
			(patternUnary 							nil bold) 
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
			(methodTempBar 						black)
			(blockTempBar 						black)
			(blockArgsBar 							black)
										
			(externalCallType 						black)
			(externalCallTypePointerIndicator 		black)
			(primitiveOrExternalCallStart 			black bold)
			(primitiveOrExternalCallEnd 			black bold)
			(primitive 								(green muchDarker))
			(pragmaKeyword 						(green muchDarker))
			(pragmaUnary 							(green muchDarker))
			(pragmaBinary 							(green muchDarker)) 
			(externalFunctionCallingConvention 	(green muchDarker) bold)
			(module 								(green muchDarker) bold))
]

{ #category : #attributes }
SHTextStylerST80 class >> textAttributesByPixelHeight [
	^ textAttributesByPixelHeight
		ifNil: [textAttributesByPixelHeight := Dictionary new]
]

{ #category : #styles }
SHTextStylerST80 class >> vintageStyleTable [
	"color can be a valid argument to Color class>>colorFrom: , or nil to
	use the editor text color.
	Multiple emphases can be specified using an array e.g. #(bold italic).
	If emphasis is not specified, #normal will be used.
	if pixel height is not specified , then the editor font size will be used.
	
	This style is based on old theme and other old environments.  
	"
	<styleTable: 'Vintage'>
								
 ^ #(
			"(symbol color [emphasisSymbolOrArray [textStyleName [pixelHeight]]])" 
			(default 								black)
			(invalid 									red)
			(excessCode 							red)
			
			(comment 								'006400')
			(unfinishedComment 					(red muchDarker))
			
			(#'$' 									('801f91' lighter))
			(character 								('801f91' lighter))
			(integer 								('801f91'))
			(number 								('801f91'))
			(#- 										('801f91'))
			(symbol 								('801f91')) 
			(stringSymbol 							('801f91')) 
			(literalArray 							black)
			(string 									('801f91'))
												
			(unfinishedString 						red normal)
			(assignment 							nil bold)
			(ansiAssignment 						nil bold)
										
			(literal 									nil italic)
												
			(keyword 								black)
			(binary 									black) 
			(unary 									black)
										
			(#true 									('A31515' darker))
			(#false 									('A31515' darker))
			(#nil 									('A31515' darker))
													
			(#self 									'A31515')
			(#super								'A31515')
			(#thisContext 							'A31515')
			(#return 								'A31515' bold)
												
			(patternArg 							(blue muchDarker)) 
			(methodArg 							(blue muchDarker))
			(blockPatternArg 						(blue muchDarker))
			(blockArg 								(blue muchDarker))
			(argument 								(blue muchDarker))
			(blockTempVar 						(blue muchDarker))
			(blockPatternTempVar 					(blue muchDarker))
								
			(instVar 								(blue muchDarker))
			(workspaceVar 							(blue muchDarker) bold)
										
			(tempVar 								(blue muchDarker))
			(patternTempVar 						(blue muchDarker))
										
			(poolConstant 							(blue muchDarker) bold)
			(classVar 								(blue muchDarker) bold)
			(globalVar 								black bold)
												
		
			(incompleteIdentifier 					blue italic)
			(incompleteKeyword 					blue italic)
			(incompleteBinary 						blue italic) 
			(incompleteUnary 						blue italic)
				
			(undefinedIdentifier 					red)						
			(undefinedKeyword 					red)
			(undefinedBinary 						red) 
			(undefinedUnary 						red) 
			
			(patternKeyword 						nil bold)
			(patternBinary 							nil bold)
			(patternUnary 							nil bold) 
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
			(cascadeSeparator 					black bold)
			(statementSeparator 					black bold)
			(methodTempBar 						black)
			(blockTempBar 						black)
			(blockArgsBar 							black)
										
			(externalCallType 						black)
			(externalCallTypePointerIndicator 		black)
			(primitiveOrExternalCallStart 			black bold)
			(primitiveOrExternalCallEnd 			black bold)
			(primitive 								(cyan muchDarker))
			(pragmaKeyword 						(cyan muchDarker))
			(pragmaUnary 							(cyan muchDarker))
			(pragmaBinary 							(cyan muchDarker)) 
			(externalFunctionCallingConvention 	(cyan muchDarker) bold)
			(module 								(cyan muchDarker) bold))
]

{ #category : #converting }
SHTextStylerST80 >> addAttributesFrom: attributeRuns satisfying: aTestBlock to: aText [

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

{ #category : #private }
SHTextStylerST80 >> addLinkAttributesFor: ranges in: aText to: charAttributes [
	| links |
	"TODO: put this on class side"
	links := Dictionary new.
	
	"see SHTextStylerST80 class >> #defaultStyleTable for more types"
	links at: #instVar put: [ :aString| TextVariableLink variableName: aString ].
	links at: #globalVar put: [ :aString| TextClassLink className: aString ].
	#(binary unary  keyword) do: [ :type|
		links at: type put: [ :aString| TextMethodLink selector: aString ]].
	#(pragmaUnary pragmaBinary patternBinary patternUnary patternKeyword) do: [ :type|
		links at: type put: [ :aString| TextMethodLink selector: aString senders: true]].
	links at: #true put: [ :aString| TextClassLink class: True ].
	links at: #false put: [ :aString| TextClassLink class: False ].
	links at: #nil put: [ :aString| TextClassLink class: UndefinedObject ].
	links at: #character put: [ :aString| TextClassLink class: Character ].
	
	"set the attrinbutes on the text"
	ranges do: [ :range|
		links at: range type ifPresent: [ :action| |attributes|
			attributes := charAttributes at: range start.
			attributes := attributes, { action cull: (range stringIn: aText) }.
			charAttributes 
				from: range start
				to: range end
				put: attributes ]]
]

{ #category : #private }
SHTextStylerST80 >> attributesFor: aSymbol [ 
	^ self class attributesFor: aSymbol pixelHeight: self pixelHeight
]

{ #category : #accessing }
SHTextStylerST80 >> classOrMetaClass: aBehavior [ 
	classOrMetaClass := aBehavior
]

{ #category : #accessing }
SHTextStylerST80 >> environment: anObject [ 
	environment := anObject
]

{ #category : #accessing }
SHTextStylerST80 >> font: aFont [ 
	font := aFont
]

{ #category : #accessing }
SHTextStylerST80 >> isForWorkspace [
	^ isForWorkspace ifNil: [ workspace notNil ]
]

{ #category : #accessing }
SHTextStylerST80 >> isForWorkspace: aBoolean [
	isForWorkspace := aBoolean
]

{ #category : #private }
SHTextStylerST80 >> pixelHeight [
	^ pixelHeight
		ifNil: [pixelHeight := (font
						ifNil: [TextStyle defaultFont]) pixelSize]
]

{ #category : #private }
SHTextStylerST80 >> privateStyle: aText [ 
	(self rangesIn: aText setWorkspace: true)
		ifNotNil: [ :ranges| self setAttributesIn: aText fromRanges: ranges].
	^aText.
]

{ #category : #private }
SHTextStylerST80 >> rangesIn: aText setWorkspace: aBoolean [ 
	parser ifNil: [parser := SHParserST80 new on: (view ifNotNil: [ view model ]) ].
	^ parser
		rangesIn: aText asString
		classOrMetaClass: classOrMetaClass
		workspace: (aBoolean
				ifTrue: [workspace]) 
		environment: environment
]

{ #category : #private }
SHTextStylerST80 >> setAttributesIn: aText fromRanges: ranges [ 
	"use the . to find and assign TextAttributes to aText"
	
	| charAttr oldRuns |
	
	oldRuns := aText runs.
	charAttr := Array new: aText size withAll: (self attributesFor: #default).
	
	ranges do: [:range | 
		(self attributesFor: range type) ifNotNil: [ :attribute|
			charAttr 
				from: range start
				to: range end
				put: attribute]].

	self addLinkAttributesFor: ranges in: aText to: charAttr.
	
	aText runs: (RunArray newFrom: charAttr).
	
	self 
		addAttributesFrom: oldRuns 
		satisfying: [:each | each shoutShouldPreserve] 
		to: aText.
]

{ #category : #converting }
SHTextStylerST80 >> unstyledTextFrom: aText [ 
	"return a Text object without all styling attributes"
	| answer |
	answer := super unstyledTextFrom: aText.
	
	self 
		addAttributesFrom: aText runs 
		satisfying: [ :each| each isKindOf: TextAction ]
		to: answer.
	
	^ answer
]

{ #category : #accessing }
SHTextStylerST80 >> workspace: aWorkspace [ 
	workspace := aWorkspace.
	isForWorkspace ifNil: [ isForWorkspace := true ]
]
