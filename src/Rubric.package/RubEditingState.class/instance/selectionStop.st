accessing
selectionStop
	^ self pointBlock ifNotNil: [:pb | pb max: self markBlock]