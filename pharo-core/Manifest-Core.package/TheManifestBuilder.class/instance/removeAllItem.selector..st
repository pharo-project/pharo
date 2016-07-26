private
removeAllItem: aSet selector: selector
	| set arrayString |
	set := (self manifest perform: selector) asOrderedCollection.
	set := set
		reject: [ :each | 
			| tmp |
			tmp := (self browsedEnvironment at: each first first) manifestReadOn: (each first at: 2).
			aSet anySatisfy: [ :fp | tmp = fp asRingDefinition ] ].
	arrayString := self buildArrayString: set.
	self compileSelector: selector returnValue: arrayString