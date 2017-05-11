examples
exampleClasses

	^ self new 
		pattern: 'mph';
		elements: (Smalltalk allClassesAndTraits collect: #name);
		open