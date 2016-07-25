widget API
performHeaderActionSelector

	^ self headerAction
		ifNil: [ nil ]
		ifNotNil: [ #performHeaderAction ]