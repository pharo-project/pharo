tests
testClassSlotDuplicationConflict

	aClass := self make: [ :builder | 
		builder classSlots: { #a } ].

	self should: [ 
		self make: [ :builder | 
			builder 
				superclass: aClass;
				name: self anotherClassName;
				classSlots: { #a } ] ]
		raise: DuplicatedSlotName 