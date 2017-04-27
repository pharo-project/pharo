examples
examplePackages

	^ self new 
		pattern: '-st';
		elements: (RPackage organizer packages collect: #name);
		open