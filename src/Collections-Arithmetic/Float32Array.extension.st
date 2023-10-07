Extension { #name : 'Float32Array' }

{ #category : '*Collections-arithmetic' }
Float32Array >> * anObject [
	^self copy *= anObject
]

{ #category : '*Collections-arithmetic' }
Float32Array >> *= anObject [
	^anObject isNumber
		ifTrue:[self primMulScalar: anObject asFloat]
		ifFalse:[self primMulArray: anObject]
]

{ #category : '*Collections-arithmetic' }
Float32Array >> + anObject [
	^self copy += anObject
]

{ #category : '*Collections-arithmetic' }
Float32Array >> += anObject [
	^ anObject addAssignToFloatArray: self
]

{ #category : '*Collections-arithmetic' }
Float32Array >> - anObject [
	^self shallowCopy -= anObject
]

{ #category : '*Collections-arithmetic' }
Float32Array >> -= anObject [
	^anObject isNumber
		ifTrue:[self primSubScalar: anObject asFloat]
		ifFalse:[self primSubArray: anObject]
]

{ #category : '*Collections-arithmetic' }
Float32Array >> / anObject [
	^self copy /= anObject
]

{ #category : '*Collections-arithmetic' }
Float32Array >> /= anObject [
	^anObject isNumber
		ifTrue:[self primDivScalar: anObject asFloat]
		ifFalse:[self primDivArray: anObject]
]

{ #category : '*Collections-arithmetic' }
Float32Array >> \\= other [

	other isNumber ifTrue: [
		1 to: self size do: [:i |
			self at: i put: (self at: i) \\ other
		].
		^ self.
	].
	1 to: (self size min: other size) do: [:i |
		self at: i put: (self at: i) \\ (other at: i).
	]
]

{ #category : '*Collections-arithmetic' }
Float32Array >> adaptToNumber: rcvr andSend: selector [
	"If I am involved in arithmetic with a Number. If possible,
	convert it to a float and perform the (more efficient) primitive operation."
	selector == #+ ifTrue:[^self + rcvr].
	selector == #* ifTrue:[^self * rcvr].
	selector == #- ifTrue:[^self negated += rcvr].
	selector == #/ ifTrue:[
		"DO NOT USE TRIVIAL CODE
			^self reciprocal * rcvr
		BECAUSE OF GRADUAL UNDERFLOW
		self should: (1.0e-39 / (Float32Array with: 1.0e-39)) first < 2."
			^(self class new: self size withAll: rcvr) / self
		].
	^super adaptToNumber: rcvr andSend: selector
]

{ #category : '*Collections-arithmetic' }
Float32Array >> negated [
	^self copy *= -1
]
