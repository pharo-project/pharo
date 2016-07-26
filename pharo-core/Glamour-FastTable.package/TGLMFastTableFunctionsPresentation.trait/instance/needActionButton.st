testing
needActionButton
	^ self parameters at: 'actionBlock' ifPresent: [ true ] ifAbsent: [ false ]