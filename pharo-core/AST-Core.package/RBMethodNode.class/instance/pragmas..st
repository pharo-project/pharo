accessing
pragmas: aCollection
	pragmas := aCollection.
	pragmas do: [ :each | each parent: self ]