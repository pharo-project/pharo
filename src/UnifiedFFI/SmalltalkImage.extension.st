Extension { #name : #SmalltalkImage }

{ #category : #'*UnifiedFFI' }
SmalltalkImage >> getSystemAttribute: aNumber [
	^ self vm getSystemAttribute: aNumber
]

{ #category : #'*UnifiedFFI' }
SmalltalkImage >> platformName [
	^ self os platformName
]
