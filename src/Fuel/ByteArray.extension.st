Extension { #name : #ByteArray }

{ #category : #'*Fuel' }
ByteArray >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitBytesObject: self
]
