tests
testSlotDuplicationConflict

	aClass := self make: [ :builder | 
		builder slots: { #a } ].

	self should: [ 
		self make: [ :builder | 
			builder 
				superclass: aClass;
				name: self anotherClassName;
				slots: { #a } ] ]
		raise: DuplicatedSlotName 