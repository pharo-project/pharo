Extension { #name : #Dictionary }

{ #category : #'*Fuel-Core' }
Dictionary >> fuelAccept: aGeneralMapper [
	| class |
	"Since we have subclasses of Dictionary that behave differently, we cannot use the visitDictionary: for all of them.	 We could also use MethodDictionary for this case, but its materialization is much slower with this cluster than with the default action."
	class := self class.
	^ (((class == Dictionary) or: [class == IdentityDictionary ])) 
		ifTrue: [ aGeneralMapper visitDictionary: self ]
		ifFalse: [ super fuelAccept: aGeneralMapper ] 
	
]

{ #category : #'*Fuel-Core' }
Dictionary >> fuelAfterMaterialization [
	| class |
	"Since for Set and IdentitySet we are recreating the collection with #add: we do not need to rehash."
	class := self class.
	^ (((class == Dictionary) or: [ class == IdentityDictionary ]) ) 
		ifFalse: [ self rehash ] 
		ifTrue: [ self ]
]
