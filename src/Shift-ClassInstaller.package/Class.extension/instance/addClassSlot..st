*Shift-ClassInstaller
addClassSlot: aSlot

 	<localClassMethod> 
   "This method is supposed to be local in Class because of a good reason.
   We use this pragma to test if Class does not contain some accidental 
   local selectors."

	^self classInstaller update: self to: [ :builder |
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