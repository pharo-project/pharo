private-scanning
scanSymbol
	[ characterType = #alphabetic or: [ currentCharacter = $: ] ] whileTrue: [
		self scanName.
		currentCharacter = $: ifTrue: [
			buffer nextPut: $:.
			self step ] ].
	^ RBLiteralToken 
		value: buffer contents asSymbol
		start: tokenStart
		stop: self previousStepPosition