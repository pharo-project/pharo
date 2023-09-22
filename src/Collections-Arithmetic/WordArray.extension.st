Extension { #name : 'WordArray' }

{ #category : '*Collections-arithmetic' }
WordArray >> * other [
	| result |
	other isNumber
		ifTrue: [ result := (other isFloat
				ifTrue: [ Float32Array ]
				ifFalse: [ WordArray ]) new: self size.
			^ self primMulScalar: self and: other into: result ].
	(other isMemberOf: WordArray)
		ifTrue: [ result := WordArray new: self size.
			^ self primMulArray: self and: other into: result ].
	(other isMemberOf: Float32Array)
		ifTrue: [ result := Float32Array new: self size.
			^ self primMulArray: self and: other into: result ].
	^ super * other
]

{ #category : '*Collections-arithmetic' }
WordArray >> + other [
	| result |
	other isNumber
		ifTrue: [ result := (other isFloat
				ifTrue: [ Float32Array ]
				ifFalse: [ WordArray ]) new: self size.
			^ self primAddScalar: self and: other into: result ].
	(other isMemberOf: WordArray)
		ifTrue: [ result := WordArray new: self size.
			^ self primAddArray: self and: other into: result ].
	(other isMemberOf: Float32Array)
		ifTrue: [ result := Float32Array new: self size.
			^ self primAddArray: self and: other into: result ].
	^ super + other
]

{ #category : '*Collections-arithmetic' }
WordArray >> - other [
	| result |
	other isNumber
		ifTrue: [ result := (other isFloat
				ifTrue: [ Float32Array ]
				ifFalse: [ WordArray ]) new: self size.
			^ self primSubScalar: self and: other into: result ].
	(other isMemberOf: WordArray)
		ifTrue: [ result := WordArray new: self size.
			^ self primSubArray: self and: other into: result ].
	(other isMemberOf: Float32Array)
		ifTrue: [ result := Float32Array new: self size.
			^ self primSubArray: self and: other into: result ].
	^ super - other
]

{ #category : '*Collections-arithmetic' }
WordArray >> / other [
	| result |
	other isNumber
		ifTrue: [ result := (other isFloat
				ifTrue: [ Float32Array ]
				ifFalse: [ WordArray ]) new: self size.
			^ self primDivScalar: self and: other into: result ].
	(other isMemberOf: WordArray)
		ifTrue: [ result := WordArray new: self size.
			^ self primDivArray: self and: other into: result ].
	(other isMemberOf: Float32Array)
		ifTrue: [ result := Float32Array new: self size.
			^ self primDivArray: self and: other into: result ].
	^ super / other
]
