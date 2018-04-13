Extension { #name : #LargePositiveInteger }

{ #category : #'*Fuel-Core' }
LargePositiveInteger >> fuelAccept: aGeneralMapper [
	^ self <= 4294967295 
		ifTrue: [ aGeneralMapper  mapAndTraceByClusterName: self to: FLPositive32SmallIntegerCluster ]
		ifFalse: [ aGeneralMapper visitBytesObject: self]
]
