Extension { #name : #Symbol }

{ #category : #'*Glamour-Helpers' }
Symbol >> asGlamourOriginIdentifier [
	^ GLMPortIdentifier defaultOriginOf: self
]

{ #category : #'*Glamour-Helpers' }
Symbol >> asGlamourTargetIdentifier [
	^ GLMPortIdentifier defaultTargetOf: self
]

{ #category : #'*Glamour-Helpers' }
Symbol >> glamourValueWithArgs: anArray [ 
	 
	anArray size < 1 ifTrue: [^nil]. 
	^anArray first perform: self
]
