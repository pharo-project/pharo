Extension { #name : #CompiledMethod }

{ #category : #'*System-Support' }
CompiledMethod >> implementors [
	^ SystemNavigation default allImplementorsOf: self selector
]

{ #category : #'*System-Support' }
CompiledMethod >> referencedClasses [
	"Return classes that are directly referenced by this method. It traverses all the compiled methods to get the classes"
	| result |
	result := IdentitySet new.
	self withAllNestedLiteralsDo: [:each | 
		each value isClass ifTrue: [ result add: each value ]].
	^result


]

{ #category : #'*System-Support' }
CompiledMethod >> senders [
	^ SystemNavigation default allSendersOf: self selector
]
