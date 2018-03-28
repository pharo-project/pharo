Extension { #name : #ByteArray }

{ #category : #'*Fuel-Core' }
ByteArray >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitBytesObject: self
]
