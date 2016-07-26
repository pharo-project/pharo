accessing selection
primarySelectionColor
	^ self primarySelection ifNil: [ Color transparent] ifNotNil: [:ps | ps color ]