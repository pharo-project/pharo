accessing
constructor

	| constructor |
	constructor := MetacelloToolBoxConstructor new.
	constructor configuration: project configuration class new.
	^constructor