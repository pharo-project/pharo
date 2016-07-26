private-scanning
scanSpecialCharacter
	| character |
	currentCharacter = $: 
		ifTrue: [
			self step.
			^ currentCharacter = $= 
				ifTrue: [
					self step.
					RBAssignmentToken start: tokenStart]
				ifFalse: [ RBSpecialCharacterToken value: $: start: tokenStart ]].
			
	currentCharacter = $_ ifTrue: [ 
		self step.  
		^ RBShortAssignmentToken start: tokenStart ].
	
	character := currentCharacter.
	self step.
	^ RBSpecialCharacterToken value: character start: tokenStart