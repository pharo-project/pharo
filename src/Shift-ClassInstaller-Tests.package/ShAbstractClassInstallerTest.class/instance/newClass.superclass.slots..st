instance creation
newClass: className superclass: aSuperclass slots: slots
	^ ShiftClassInstaller
		make: [ :builder | 
			builder
				name: className;
				superclass: aSuperclass;
				slots: slots;
				sharedVariables: '';
				sharedPools: '';
				category: 'Shift-ClassInstaller-Tests';
				copyClassSlotsFromExistingClass ]