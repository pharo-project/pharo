accessing
asSelectorEnvironment
	| result |
	result := RBSelectorEnvironment onEnvironment: environment.
	self classesAndSelectorsDo: [ :class :selector | result addClass: class selector: selector ].
	^ result