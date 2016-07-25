accessing
messageText
	^ streamPosition 
		ifNil: [ 
			super messageText ] 
		ifNotNil: [ :pos | 
			'At character {1}: {2}' format: 
				(Array with: streamPosition with: super messageText) ]