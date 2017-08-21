*Slot
removeSlot: aSlot 

	(self classLayout slots includes: aSlot)
		ifFalse: [self error: aSlot name , ' is not one of my slots'].
		
	^self classInstaller make: [ :builder |
		builder
			superclass:  self superclass;
			name:  self name;
			layoutClass: self classLayout class;
			slots: (self classLayout slots copyWithout: aSlot);
			classSlots: self class classLayout slots;
			sharedVariablesFromString: self classVariablesString;
			sharedPools: self sharedPoolsString;
			category: self category;
			environment: self environment]
	