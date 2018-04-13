Extension { #name : #SmallInteger }

{ #category : #'*Fuel-Core' }
SmallInteger >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitSmallInteger: self
]

{ #category : #'*Fuel-Core-Collections' }
SmallInteger >> largeIdentityHash [
	"Return an integer between 0 and 4095 based on all of my bits. Make sure that for nearby receivers the result will not be nearby with high probability."

	| remainder hash |
	self >= 0 
		ifTrue: [
			remainder := self.
			hash := 0 ]
		ifFalse: [
			remainder := 0 - self.
			hash := 4095 ].
	[ 
		hash := hash bitXor: (remainder bitAnd: 4095).
		(remainder := remainder bitShift: -12) = 0 ] whileFalse.
	^FLLargeIdentityHashedCollection permuteHash: hash + 1
]
