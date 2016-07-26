colorspace conversion
sampleRangeLimit: aNumber

	aNumber < 0 ifTrue: [^ 0].
	aNumber > MaxSample ifTrue: [^ MaxSample].
	^ aNumber