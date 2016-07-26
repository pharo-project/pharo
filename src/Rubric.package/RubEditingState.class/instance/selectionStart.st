accessing
selectionStart
	^ self pointBlock ifNotNil: [:pb | pb min: self markBlock]