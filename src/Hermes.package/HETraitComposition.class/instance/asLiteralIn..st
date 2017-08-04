converting
asLiteralIn: env
	^ TraitComposition new
		transformations: (value collect: [ :e | e asLiteralIn: env ]) asOrderedCollection;
		yourself