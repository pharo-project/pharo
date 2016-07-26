private
computeLength
	| width size length |

	size := tabs size.
	size isZero ifTrue: [ ^ 0 ].
	width := self width - self extraSpace + ((size-1) * self overlap).
	
	length := width / size.
	length := length min: 150.
	length := length max: 33.

	^ length
	