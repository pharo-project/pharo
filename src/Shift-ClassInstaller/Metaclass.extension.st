Extension { #name : #Metaclass }

{ #category : #'*Shift-ClassInstaller' }
Metaclass >> instanceVariableNames: instVarString [ 
	"Declare additional named variables for my instance."
	| theClass |
	theClass := self theNonMetaClass.
	
	theClass := theClass classInstaller update: theClass to: [ :builder |
		builder
			superclass: theClass superclass;
			name: theClass name;
			layoutClass: theClass classLayout class;
			slots: theClass slots;
			sharedVariablesFromString: theClass classVariablesString;
			sharedPools: theClass sharedPoolsString;
			traitComposition: theClass traitComposition;
			category: theClass category asString;
			classSlots: instVarString asSlotCollection ].
	^ theClass theMetaClass
]
