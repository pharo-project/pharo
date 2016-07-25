private
replace: string from: start to: stop with: replacement startingAt: repStart
	string isByteString
		ifTrue: [ 
			string replaceFrom: start to: stop with: replacement startingAt: repStart ]
		ifFalse: [ 
			| index repOff |
			repOff := repStart - start.
			index := start - 1.
			[ (index := index + 1) <= stop ] 
				whileTrue: [ 
					string at: index put: (replacement at: repOff + index) asCharacter ] ]