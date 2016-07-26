private
sourceOf: aSHRange stopAt: aNumber
	^ aSHRange type = #empty
		ifTrue: [String new]
		ifFalse: [source copyFrom: aSHRange start to: aNumber]