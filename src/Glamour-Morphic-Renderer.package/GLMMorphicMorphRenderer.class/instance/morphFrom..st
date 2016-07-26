rendering
morphFrom: aMorphPresentation
	| morph  |
	
	morph := aMorphPresentation morph 
		vResizing: #spaceFill; 
		hResizing: #spaceFill;
		yourself.
	
	(self allKeyboardActionsFor: aMorphPresentation) ifNotEmpty: [
		morph 
			on: #keyStroke 
			send: #actOnKeyStroke:event:from: 
			to: self 
			withValue: aMorphPresentation ].
		
	^morph