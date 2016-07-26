instance creation
first: aShortcut next: anotherShortcut
	^ (self new)
		first: aShortcut;
		next: anotherShortcut;
		yourself