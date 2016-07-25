formatting
formatSourceCode
	self
		handleEdit: [ 
			| source tree formatted |
			source := self textMorph text asString.
			tree := RBParser parseMethod: source onError: [ :msg :pos | ^ self ].
			formatted := tree formattedCode.
			formatted = source
				ifTrue: [ ^ self ].
			self
				editString: formatted;
				hasUnacceptedEdits: true ]