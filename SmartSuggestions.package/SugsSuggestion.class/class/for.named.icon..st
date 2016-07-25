instance creation
for: blockCommand named: label icon: icon 
	^ self new 
		blockCommand: blockCommand;
		label: label;
		icon: icon;
		yourself.