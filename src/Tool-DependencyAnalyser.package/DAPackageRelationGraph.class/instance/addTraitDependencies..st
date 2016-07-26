adding
addTraitDependencies: aPackage
	| package |
	self package: aPackage
		classesDo: [ :class | 
			class traits
				do: [ :trait |
					package := self packageForBehavior: trait.
					aPackage
						add:
							((DATraitDependency from: aPackage to: package)
								theClass: class;
								trait: trait).
					self addPackage: package ] ]