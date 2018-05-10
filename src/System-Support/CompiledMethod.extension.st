Extension { #name : #CompiledMethod }

{ #category : #'*System-Support' }
CompiledMethod >> implementors [
	^ SystemNavigation default allImplementorsOf: self selector
]

{ #category : #'*System-Support' }
CompiledMethod >> referencedClasses [
	"Return classes that are directly referenced by this method"

	^ self literals allButLast
		select: [ :l | l value isClass ] 
		thenCollect: [:v | v value ].
]

{ #category : #'*System-Support' }
CompiledMethod >> senders [
	^ SystemNavigation default allSendersOf: self selector
]
