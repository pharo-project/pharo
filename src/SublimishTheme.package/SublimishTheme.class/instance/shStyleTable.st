defaults
shStyleTable

	"color can be a valid argument to Color class>>colorFrom: , or nil to
	use the editor text color.
	Multiple emphases can be specified using an array e.g. #(bold italic).
	If emphasis is not specified, #normal will be used.
	if pixel height is not specified , then the editor font size will be used.
	
	This style is inspired on darkula theme from eclipse: http://eclipsecolorthemes.org/?view=theme&id=15515.  
	"
	<styleTable: 'Sublimish'>

 ^ #(
			"(symbol color [emphasisSymbolOrArray [textStyleName [pixelHeight]]])" 
			(default 								white)
			(invalid 								'EE3F08' bold)
			(excessCode 							'EE3F08' bold )
			
			(comment 								'72725e' italic)
			(unfinishedComment 				'72725e')
			
			(#'$' 									'ef29ef')
			(character 							'ef29ef')
			(integer 								'E8E187')
			(number 								'E8E187')
			(#- 									('8ae234'))
			(symbol 								('FFA180')) 
			(stringSymbol 						('FFA180')) 
			(literalArray 						white)
			(string 								('E8E187'))
			"(string 								('babdb6'))"
												
			(unfinishedString 					'babdb6' italic)
			(assignment 							nil bold)
			(ansiAssignment 					nil bold)
										
			(literal 								nil italic)
												
			"(keyword 								'729fcf')
			(binary 								'729fcf') 
			(unary 								'729fcf')"
			(keyword 								'EBEBEB')
			(binary 								'EBEBEB') 
			(unary 								'EBEBEB')
										
			(#true 								'9ffc3d' )
			(#false 								'9ffc3d' )
			(#nil 									'9ffc3d' )
													
			(#self 								'ECC284' bold)
			(#super								'ECC284' bold)
			(#thisContext 						'ECC284' bold)
			(#return 								'ECC284' bold)
												
			(patternArg 							'ECC284' italic) 
			(methodArg 							'ECC284' italic)
			(blockPatternArg 					'ECC284' italic)
			(blockArg 							'ECC284' italic)
			(argument 							'ECC284' italic)
			(blockTempVar 						'ECC284' italic)
			(blockPatternTempVar 				'ECC284' italic)
								
			(instVar 								'ECC284')
			(workspaceVar 						'ECC284' italic)
										
			(tempVar 								'ECC284' italic)
			(patternTempVar 					'ECC284' italic)
										
			(poolConstant 						'A7E2ED')
			(classVar 							'CEEAA7')
			"(globalVar 							'fcaf3e' bold)"
			(globalVar 							'FFC165')
		
			(incompleteIdentifier 			'E8E2B7' italic)
			(incompleteKeyword 				'E8E2B7' italic)
			(incompleteBinary 					'E8E2B7' italic) 
			(incompleteUnary 					'E8E2B7' italic)
				
			(undefinedIdentifier 				'EE3F08' bold)						
			(undefinedKeyword 					'EE3F08' bold)
			(undefinedBinary 					'EE3F08' bold) 
			(undefinedUnary 					'EE3F08' bold) 
			
			(patternKeyword 					'729fcf' bold)
			(patternBinary 						'729fcf' bold)
			(patternUnary 						'729fcf' bold) 
			(blockArgColon 						'ECC284' italic)
			(leftParenthesis 					white)
			(rightParenthesis 					white)
			(leftParenthesis1 					(green muchLighter))
			(rightParenthesis1 				(green muchLighter))
			(leftParenthesis2 					(magenta muchLighter))
			(rightParenthesis2 				(magenta muchLighter))
			(leftParenthesis3 					(red muchLighter))
			(rightParenthesis3 				(red muchLighter))
			(leftParenthesis4 					(green lighter))
			(rightParenthesis4 				(green lighter))
			(leftParenthesis5 					(orange lighter))
			(rightParenthesis5 				(orange lighter))
			(leftParenthesis6 					(magenta lighter))
			(rightParenthesis6 				(magenta lighter))
			(leftParenthesis7 					blue)
			(rightParenthesis7 				blue)
			(blockStart 							white bold)
			(blockEnd 							white bold)
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
			(pragmaUnary 									'FF3333')
			(pragmaBinary 									'B4DD6E') 
			(externalFunctionCallingConvention 		'B4DD6E' bold)
			(module 											'B4DD6E' bold))