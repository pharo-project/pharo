Extension { #name : 'Float' }

{ #category : '*DeepCopier' }
Float >> veryDeepCopyWith: deepCopier [
	"Return self.  Do not record me."

	^ self shallowCopy
]
