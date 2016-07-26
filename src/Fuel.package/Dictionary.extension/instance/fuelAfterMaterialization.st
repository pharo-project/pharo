*Fuel
fuelAfterMaterialization
	| class |
	"Since for Set and IdentitySet we are recreating the collection with #add: we do not need to rehash."
	class := self class.
	^ (((class == Dictionary) or: [ class == IdentityDictionary ]) ) 
		ifFalse: [ self rehash ] 
		ifTrue: [ self ]