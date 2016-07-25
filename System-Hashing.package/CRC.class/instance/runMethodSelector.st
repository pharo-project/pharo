private-run-methods
runMethodSelector
	^ runMethodSelector ifNil: [ 
		runMethodSelector := 
			shouldReflectInput
				ifTrue: [ 
					shouldReflectOutput
						ifTrue: [ #runRefInRefOut ] 
						ifFalse: [ #runRefInNonRefOut ] ]
				ifFalse: [ 
					shouldReflectOutput 
						ifTrue: [ #runNonRefInRefOut ] 
						ifFalse: [ #runNonRefInNonRefOut ] ] ]