initialize-release
classes: classArray metaClasses: metaArray 
	"Used to recreate an environment from its storeString"

	classSelectors := IdentityDictionary new.
	metaClassSelectors := IdentityDictionary new.
	classArray 
		do: [ :each | classSelectors at: each first put: each last asIdentitySet ].
	metaArray 
		do: [ :each | metaClassSelectors at: each first put: each last asIdentitySet ]