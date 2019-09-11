Extension { #name : #SmalltalkImage }

{ #category : #'*UnifiedFFI' }
SmalltalkImage >> getSystemAttribute: aNumber [
	^ self vm getSystemAttribute: aNumber
]
