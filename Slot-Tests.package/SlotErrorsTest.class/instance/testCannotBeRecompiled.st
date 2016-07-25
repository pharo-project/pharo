tests
testCannotBeRecompiled

	| superclass |
	superclass := self make: [ :builder | 
		builder 
			superclass: Object;
			name: self aClassName ].

	self make: [ :builder | 
		builder 
			superclass: superclass;
			name: self anotherClassName ].

	self 
		should: [ 
			self make: [ :builder | 
				builder 
					layoutClass: VariableLayout;
					name: self aClassName ] ]
		raise: Error
		description: 'Old class builder raises: X cannot be recompiled'
