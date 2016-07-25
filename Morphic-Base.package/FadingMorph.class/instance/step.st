stepping and presenter
step

	alpha <= fadingFactor ifTrue: [ ^self delete ].

	alpha := alpha - fadingFactor.
	
	self changed.

	
	