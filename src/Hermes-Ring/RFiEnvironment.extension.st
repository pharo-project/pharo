Extension { #name : #RFiEnvironment }

{ #category : #'*Hermes-Ring' }
RFiEnvironment >> bindingOf: aName [
	
	^ self 
		at: aName
		ifPresent: [ :value | GlobalVariable key: aName value: value ]
		ifAbsent: [ nil ]
]
