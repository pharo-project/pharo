name lookup
includesKey: aName
	"does this pool include aName"
	^(self bindingOf: aName) notNil