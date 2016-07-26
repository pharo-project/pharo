initialize-release
slots: slotsDefinition 
	"Declare additional named variables for my instance."
	| theClass |
	theClass := self theNonMetaClass.
	
	theClass := theClass classInstaller make: [ :builder |
		builder
			superclass: theClass superclass;
			name: theClass name;
			layoutClass: theClass classLayout class;
			slots: theClass slots;
			sharedVariablesFromString: theClass classVariablesString;
			sharedPools: theClass sharedPoolsString;
			traitComposition: theClass traitComposition;
			category: theClass category asString;
			classSlots: slotsDefinition].
	^ theClass theMetaClass