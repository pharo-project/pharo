examples
someSmallSmalltalkCode.
	^ '
WorldState
	addDeferredUIMessage: [ 
		World displayWorldSafely.
		Cursor wait
			showWhile: [ 
				aBlock
					ensure: [ m delete ] ] ]'