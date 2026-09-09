Extension { #name : 'Float' }

{ #category : '*System-Model' }
Float >> veryDeepCopyWith: deepCopier [
	"Return self.  Do not record me."

	^ self shallowCopy
]
