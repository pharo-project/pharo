tests
testGettingAbsentAttribute

	| actual |

	textModel := TxModel new.
	textModel defaultStyle at: TxFontAttribute put: #mockValue.
	span text: textModel.
	
	actual := span getAttribute: TxFontAttribute.
	
	self assert: actual equals: #mockValue