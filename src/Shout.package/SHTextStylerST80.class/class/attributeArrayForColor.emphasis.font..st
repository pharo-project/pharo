style table
attributeArrayForColor: aColorOrNil emphasis: anEmphasisSymbolOrArrayorNil font: aTextStyleOrFontOrNil 

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