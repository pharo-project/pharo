*Fuel
fuelAfterMaterialization

	"Since for Set and IdentitySet we are recreating the collection with #add: (see FLSimpleCollectionCluster) we do not need to rehash"
	^ ((self class == Set) or: [ self class == IdentitySet ] ) 
		ifFalse: [ self rehash ] 
		ifTrue: [ self ]