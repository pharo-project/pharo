helpers
should: superclassBlock extendWith: subclassBlock

	| superclass |
	superclass := self make: superclassBlock.

	self make: [ :builder | 
		builder 
			superclass: superclass;
			name: self anotherClassName;
			in: subclassBlock ]