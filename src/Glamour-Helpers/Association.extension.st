Extension { #name : #Association }

{ #category : #'*Glamour-Helpers' }
Association >> asGlamourOriginIdentifier [
	^ GLMPortIdentifier new 
		paneName: self key;
		portName: self value
]

{ #category : #'*Glamour-Helpers' }
Association >> asGlamourTargetIdentifier [
	^ GLMPortIdentifier new 
		paneName: self key;
		portName: self value
]
