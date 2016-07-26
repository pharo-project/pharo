helpers
shouldnt: superclassBlock extendWith: subclassBlock

	| superclass |
	superclass := self make: superclassBlock.

	self should: [
		self make: [ :builder | 
			builder 
				superclass: superclass;
				name: self anotherClassName;
				in: subclassBlock ] ]
		raise: IncompatibleLayoutConflict