private
smartNeedExtraRemoveIn: currentText for: smartCharacter opposite: opposite at: position
	"Test if we need to remove an extra character when removing a smart character (any kind of smart character)"
	
	smartCharacter = opposite
		ifTrue: [ 
			(self smartNeedExtraRemoveIn: currentText for: opposite)
				ifFalse: [ ^ false ] ]
		ifFalse: [ 
			(self
				smartNeedExtraRemovePairedIn: currentText
				for: smartCharacter
				opposite: opposite
				at: position)
					ifFalse: [ ^false ] ].
	^ true