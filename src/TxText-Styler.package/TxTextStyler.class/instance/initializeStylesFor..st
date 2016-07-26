private
initializeStylesFor: styleTable

	styles := IdentityDictionary new.
	
	styleTable
		do: [:each |   | stream styleName style temp |
			
			
			stream := each readStream.
			style := TxStyle new.
			
			"style name"
			styleName := stream next.
		
			"color"
			temp := stream next.
			temp ifNotNil: [
				style add: (TxForeColorAttribute with: (Color colorFrom: temp)).
			].
			
			"emphasis"	
			temp := stream next.
			temp ifNotNil: [
				style add: (TxFontAttribute with: (self pickFontWithEmphasis: temp)).
			 ].	
				
			"text style name, ignored"
			stream next.
			
			"pixel height"
			temp := stream next.
			temp ifNotNil: [ 
				"ignored by now"
				 ].
			
			styles at: styleName put: style.
		]