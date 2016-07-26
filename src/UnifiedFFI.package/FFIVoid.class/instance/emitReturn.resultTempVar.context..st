emitting code
emitReturn: aBuilder resultTempVar: resultVar context: aContext 
	self isPointer 
		ifTrue: [ ^ super emitReturn: aBuilder resultTempVar: resultVar context: aContext ].
	^ self emitSelfReturn: aBuilder