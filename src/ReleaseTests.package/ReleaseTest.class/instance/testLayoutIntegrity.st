testing
testLayoutIntegrity
	SystemNavigation default allClassesDo: [ :class |
		class theNonMetaClass classLayout checkIntegrity.
		class theMetaClass classLayout checkIntegrity ]