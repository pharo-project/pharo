menu building
menuBuilder

	^ menuBuilder ifNil: [ menuBuilder := PragmaMenuBuilder pragmaKeyword: self discoveredMenuPragmaKeyword model: self ].