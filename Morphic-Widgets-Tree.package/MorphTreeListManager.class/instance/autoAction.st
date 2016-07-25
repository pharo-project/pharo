selection change
autoAction
	^ firstClickedMorph
		ifNotNil: [
			firstClickedMorph selected
				ifTrue: [#addToSelection:]
				ifFalse: [#removeFromSelection:]]. 
	