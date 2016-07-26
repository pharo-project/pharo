instance variables
addSlot: aSlot
	^self classInstaller make: [ :builder |
		builder
			superclass: self superclass;
			name: self getName;
			layoutClass: self classLayout class;
			slots: (self slots copyWith: aSlot);
			classSlots: self class classLayout slots;
			sharedVariablesFromString: self classVariablesString;
			sharedPools: self sharedPoolsString;
			category: self category;
			environment: self environment].