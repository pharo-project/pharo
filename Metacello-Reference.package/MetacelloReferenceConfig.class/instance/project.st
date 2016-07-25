accessing
project

	^ project ifNil: [ | constructor |
		"Construct Metacello project"
		constructor := (Smalltalk at: #MetacelloVersionConstructor) on: self.
		project := constructor project ]