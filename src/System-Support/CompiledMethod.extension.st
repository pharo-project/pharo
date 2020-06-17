Extension { #name : #CompiledMethod }

{ #category : #'*System-Support' }
CompiledMethod >> implementors [
	^ SystemNavigation default allImplementorsOf: self selector
]

{ #category : #'*System-Support' }
CompiledMethod >> referencedClasses [
	"Return classes that are directly referenced by this method. It traverses all the compiled methods to get the classes"

	^ self withAllBlocks flatCollect: [ :aCompiledCode | 
			aCompiledCode literals
				select: [ :l | l value isClass ] 
				thenCollect: [:v | v value ]]


]

{ #category : #'*System-Support' }
CompiledMethod >> senders [
	^ SystemNavigation default allSendersOf: self selector
]
