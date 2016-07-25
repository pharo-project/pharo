printing
definitionString
	^ String streamContents: [:stream | self printDefinitionOn: stream]