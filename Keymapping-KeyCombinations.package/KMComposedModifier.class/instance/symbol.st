printing
symbol
	^ String streamContents: [ :stream | 
		self modifiers do: [ :e | 
			stream << e symbol ] ]