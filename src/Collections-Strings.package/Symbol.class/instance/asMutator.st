converting
asMutator
	"Return a setter message from a getter message. For example,
	#name asMutator returns #name:
	return self if it is already a setter"
	self endsWithAColon ifTrue:[ ^ self ].
	^ (self copyWith: $:) asSymbol