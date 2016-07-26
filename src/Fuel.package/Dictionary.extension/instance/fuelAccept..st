*Fuel
fuelAccept: aGeneralMapper
	| class |
	"Since we have subclasses of Dictionary that behave differently, we cannot use the visitDictionary: for all of them.	 We could also use MethodDictionary for this case, but its materialization is much slower with this cluster than with the default action."
	class := self class.
	^ (((class == Dictionary) or: [class == IdentityDictionary ])) 
		ifTrue: [ aGeneralMapper visitDictionary: self ]
		ifFalse: [ super fuelAccept: aGeneralMapper ] 
	