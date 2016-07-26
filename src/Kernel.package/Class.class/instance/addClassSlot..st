private
addClassSlot: aSlot
	^self classInstaller make: [ :builder |
		builder
			superclass: self superclass;
			name: self name;
			layoutClass: self classLayout class;
			slots: self slots;
			classSlots: (self class classLayout slots copyWith: aSlot);
			sharedVariablesFromString: self classVariablesString;
			sharedPools: self sharedPoolsString;
			category: self category;
			environment: self environment].