requirements
identityCollectionWithElementsCopyNotIdentical
"Returns a collection including elements for which #copy doesn't return the same object."
	^ identityBagNonEmptyNoDuplicate5Elements ifNil: [ 
	identityBagNonEmptyNoDuplicate5Elements := IdentityBag new add: 2.5 ; add: 1.5  ;add: 5.5 ; yourself ]