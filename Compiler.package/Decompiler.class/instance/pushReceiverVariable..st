instruction decoding
pushReceiverVariable: offset

	| var |
	(var := instVars at: offset + 1 ifAbsent: []) == nil
		ifTrue:
			["Not set up yet"
			var := constructor codeInst: offset.
			instVars size < (offset + 1) ifTrue: [
				instVars := (Array new: offset + 1)
					replaceFrom: 1 to: instVars size with: instVars; yourself ].
			instVars at: offset + 1 put: var].
	stack addLast: var