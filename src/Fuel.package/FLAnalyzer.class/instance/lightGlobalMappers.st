analyzing
lightGlobalMappers

	^ globalSymbols isEmpty
		ifTrue: [ #() ]
		ifFalse: [ Array with: (FLLightGlobalMapper for: globalSymbols in: self globalEnvironment) ]