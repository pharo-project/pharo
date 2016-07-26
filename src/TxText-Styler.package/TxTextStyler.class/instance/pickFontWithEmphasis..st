private
pickFontWithEmphasis: emphasisName
	| emphasis |
	emphasis := TextEmphasis perform: emphasisName.
	
	^ TxFontAttribute defaultValue emphasized: emphasis emphasisCode.

