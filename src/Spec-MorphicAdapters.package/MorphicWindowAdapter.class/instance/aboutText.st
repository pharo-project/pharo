widget API
aboutText

	^ self model model
		ifNotNil: [ :m | m aboutText ]