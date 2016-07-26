api-configuration
createSymbolicVersionMethod: selector inCategory: category forVersion: versionSymbol
	| constructor |
	methodSpec := (MetacelloSymbolicVersionMethodSpec new)
		project: project;
		selector: selector;
		category: category;
		versionString: versionSymbol;
		yourself.
	constructor := self constructor.
	self methodSpec methodSections: (constructor extractSymbolicVersionSpecsFor: versionSymbol) asOrderedCollection.
