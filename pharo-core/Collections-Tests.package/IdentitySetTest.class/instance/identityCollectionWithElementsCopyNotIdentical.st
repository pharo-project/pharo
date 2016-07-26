requirements
identityCollectionWithElementsCopyNotIdentical
" return a collection including elements for which #copy return a new object "
	^ floatCollection ifNil: [ floatCollection := IdentitySet new add: 2.5 ; add: 4.5 ; add:5.5 ; yourself ].