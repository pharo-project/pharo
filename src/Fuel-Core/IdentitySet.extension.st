Extension { #name : #IdentitySet }

{ #category : #'*Fuel-Core' }
IdentitySet >> fuelReplacement [
	| copy |
	copy := self copyEmpty addAll: self.
	^ (self size ~= copy size)
		ifTrue: [ copy ]
		ifFalse: [ self ]
]
