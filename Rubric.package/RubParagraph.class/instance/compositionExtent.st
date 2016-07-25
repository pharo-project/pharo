composition
compositionExtent
	"Return the bounds for composing this text.  There are two cases:
	1.  wrapped is true -- grow downward as necessary,
	2.  wrapped is false -- grow in 2D as nexessary."

	^ (self wrapped
		ifTrue: [ self container width @ SmallInteger maxVal ]
		ifFalse: [ SmallInteger maxVal @ SmallInteger maxVal ]) max: self minimumCompositionExtent