accessing
level
	^ parentNode ifNil: [1] ifNotNil: [parentNode level + 1]