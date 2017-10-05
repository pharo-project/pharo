tests
testClassWithComment
	newClass := ShiftClassInstaller
		make: [ :builder | 
			builder
				name: #SHClassWithComment;
				superclass: Object;
				slots: #();
				sharedVariables: '';
				sharedPools: '';
				category: 'Shift-ClassInstaller-Tests';
				comment: 'I have a comment'; 
				commentStamp: 'anStamp';				
				copyClassSlotsFromExistingClass ].
	
	self assert: newClass comment equals: 'I have a comment'.
	self assert: newClass organization commentStamp equals: 'anStamp'.