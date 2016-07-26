*Collections-arithmetic
+ other

	| result |
	other isNumber ifTrue: [
		other isFloat ifTrue: [
			result := FloatArray new: self size.
			^ self primAddScalar: self and: other into: result.
		] ifFalse: [
			result := WordArray new: self size.
			^ self primAddScalar: self and: other into: result.
		].
	].
	(other isMemberOf: WordArray) ifTrue: [	
		result := WordArray new: self size.
		^ self primAddArray: self and: other into: result.
	].
	(other isMemberOf: FloatArray) ifTrue: [	
		result := FloatArray new: self size.
		^ self primAddArray: self and: other into: result.
	].
	^ super + other.
