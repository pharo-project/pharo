private-focus
giveFocusToNextFrom: aModel
	| focus |
	
	focus := self focusOrder.

	(focus includes: aModel)
		ifTrue: [
			| index next |
			index := (focus indexOf: aModel) + 1.
			(index <= focus size)
				ifTrue: [
					next := focus at: index.
					next takeKeyboardFocus.
					^ true ]].

	^ owner 
		ifNil: [ " I loop "
			| next |
			next := focus at: 1 ifAbsent: [ ^ false ].
			next takeKeyboardFocus.
			true ]
		ifNotNil: [ 
			owner giveFocusToNextFrom: self.
			true ]