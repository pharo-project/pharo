accessing
numberOfRows
	^ self items
		ifNotNil: [ | cpt |
			cpt := 0.
			self items do: [ :elements | cpt := cpt + elements size ].
			cpt ]
		ifNil: [ 0 ]