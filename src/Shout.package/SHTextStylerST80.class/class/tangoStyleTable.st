style table
tangoStyleTable
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