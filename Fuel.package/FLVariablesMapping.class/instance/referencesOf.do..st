analyzing
referencesOf: anObject do: aBlock

	mapping do: [ :index | aBlock value: (anObject instVarAt: index) ].