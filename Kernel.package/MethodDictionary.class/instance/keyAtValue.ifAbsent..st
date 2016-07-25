accessing
keyAtValue: value ifAbsent: exceptionBlock
	"Answer the key whose value equals the argument, value. If there is
	none, answer the result of evaluating exceptionBlock."
	1 to: self basicSize do:
		[:index |
		value = (array at: index)
			ifTrue:
				[(self basicAt: index) ifNotNil: [ :theKey | ^ theKey]]].
	^ exceptionBlock value