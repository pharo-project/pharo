accessing
hsvSaturation
	"Return the HSV saturation of this color, a value between 0.0 and 1.0."

	| max |
	max := self privateMaxComponent.
	max isZero
		ifTrue: [ ^ 0.0 ]
		ifFalse: [ ^ (max - self privateMinComponent) asFloat / max asFloat ]