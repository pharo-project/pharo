Extension { #name : 'SHRBTextStyler' }

{ #category : '*DarkBlueTheme' }
SHRBTextStyler class >> darkBlueStyleTable [
	<styleTable: 'DarkBlue'>

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
			(#return 								'00CC6E' bold)

			(patternArg 							'03a9f4' bold)
			(argument 							'03a9f4')
										
			(instVar 								'ffa000')										

			(patternTempVar 					'00acc1')
			(tempVar 								'00acc1')
																					
			(blockPatternArg 					'0091ea' italic)
			(blockArg 							'0091ea' italic)

			(blockPatternTempVar 				'00838f' italic)
			(blockTempVar 						'00838f' italic)

			(poolConstant 						'A7E2ED' bold)
			(classVar 							'A7E2ED' bold)
			(globalVar 							white bold)


			(incompleteIdentifier 			'E8E2B7' italic)
			(incompleteSelector				'E8E2B7' italic)

			(undefinedIdentifier 				'FF8A80')
			(undefinedSelector 					'FF8A80')

			(patternSelector 						nil bold)
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
			(cascadeSeparator 					white bold)
			(statementSeparator 				white bold)
			(methodTempBar 						white)
			(blockTempBar 						white)
			(blockArgsBar 						white)

			
			(pragma 									'B4DD6E'))
]
