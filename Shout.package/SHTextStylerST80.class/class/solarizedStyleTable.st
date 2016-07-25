style table
solarizedStyleTable
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