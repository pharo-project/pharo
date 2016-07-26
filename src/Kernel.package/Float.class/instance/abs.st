arithmetic
abs
	"This is faster than using Number abs and works for negativeZero."
	self <= 0.0
		ifTrue: [^ 0.0 - self]
		ifFalse: [^ self]