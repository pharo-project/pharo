private
createSymbolTable
	| attributes |
	attributes := {TextFontReference toFont: self symbolFont}.
	^ Dictionary newFromPairs: { 
		#Cmd. 		Text string: (Character value: 16r2318) asString attributes: attributes.
		#Alt. 		Text string: (Character value: 16r2325) asString attributes: attributes. 
		#Ctrl.  	Text string: (Character value: 16r2303) asString attributes: attributes. 
		#Shift. 	Text string: (Character value: 16r21E7) asString attributes: attributes.
		"Lucida does not have the enter key"
		"#Enter. 	Text string: (Character value: 16r23ce) asString attributes: attributes" }.
