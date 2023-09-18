Extension { #name : #CompiledMethod }

{ #category : #'*System-Support' }
CompiledMethod >> implementors [
	^ SystemNavigation default allImplementorsOf: self selector
]

{ #category : #'*System-Support' }
CompiledMethod >> referencedClasses [

	"Return classes that are directly referenced by this method.
	We only take globals into account, class vars are skipped.
	It traverses all the compiled methods to get the classes"

	| result |
	result := IdentitySet new.
	self withAllNestedLiteralsDo: [ :each |
		(each isVariableBinding and: [
			 each isGlobalVariable and: [ each read isClass ] ]) ifTrue: [
			result add: each value ] ].
	^ result
]

{ #category : #'*System-Support' }
CompiledMethod >> senders [
	^ SystemNavigation default allSendersOf: self selector
]
