Extension { #name : 'SystemDictionary' }

{ #category : '*Slot-Core' }
SystemDictionary >> declare: key from: aDictionary [
	"Add key to the receiver. If key already exists, do nothing. If aDictionary
	includes key, then remove it from aDictionary and use its association as
	the element of the receiver."
	"overridden here to add a Global, not just an association"

	(self includesKey: key) ifTrue: [^ self].
	(aDictionary includesKey: key)
		ifTrue:
			[| undeclared |
			undeclared := aDictionary associationAt: key.
			"Undeclared variables record using methods in a property, remove"
			undeclared removeProperty: #usingMethods ifAbsent: [ ]. 
			"and change class to be Global"
			self add: (undeclared primitiveChangeClassTo: GlobalVariable new).
			aDictionary removeKey: key]
		ifFalse:
			[self add: (GlobalVariable key: key)]
]
