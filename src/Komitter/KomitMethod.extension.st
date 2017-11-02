Extension { #name : #KomitMethod }

{ #category : #'*Komitter-UI' }
KomitMethod >> koDestinationText [
	^ operation koDestinationText
]

{ #category : #'*Komitter-UI' }
KomitMethod >> koSourceText [
	
	^ self operation koSourceText
]

{ #category : #'*Komitter-UI' }
KomitMethod >> printOn: aStream [

	super printOn: aStream.
	aStream << '[ '.
	self method selector printOn: aStream.
	aStream << ' ]'.
]
