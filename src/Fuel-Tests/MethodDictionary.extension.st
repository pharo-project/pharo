Extension { #name : #MethodDictionary }

{ #category : #'*Fuel-Tests' }
MethodDictionary >> isEqualRegardlessMethodsTrailerTo: aMethodDictionary [

	"Two dictionaries are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same set of keys.
	 (c) for each (common) key, they have the same value"

	self == aMethodDictionary ifTrue: [ ^ true ].
	(aMethodDictionary isDictionary) ifFalse: [^false].
	self size = aMethodDictionary size ifFalse: [^false].
	self associationsDo: [:assoc|
		((aMethodDictionary at: assoc key ifAbsent: [^false]) isEqualRegardlessTrailerTo:  assoc value)
			ifFalse: [^false]].
	^true


]
