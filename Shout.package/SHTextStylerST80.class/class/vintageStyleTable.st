style table
vintageStyleTable
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