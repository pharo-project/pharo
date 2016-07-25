text
classDefinitionString
	| defs |
	defs := items select: [:ea | (ea isClassDefinition or: [ea isClassDefinitionExtension])
			and: [ea className = classSelection]].

	defs isEmpty ifTrue: [^ 'This class is defined elsewhere.'].

	^ String streamContents: [:stream | 
		defs asArray sort 
			do: [:ea | ea printDefinitionOn: stream]
			separatedBy: [stream nextPut: $.; cr]
	].