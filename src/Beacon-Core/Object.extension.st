Extension { #name : #Object }

{ #category : #'*Beacon-Core' }
Object >> asBeaconSignal [
	^ WrapperSignal on: self
]
