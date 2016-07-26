example
example
	^ PharoClassInstaller make: [ :aSlotClassBuilder |
		aSlotClassBuilder
			superclass: Object;
			name: #MyClass;
			slots: #(varA varB);
			category: 'My-Category' ].