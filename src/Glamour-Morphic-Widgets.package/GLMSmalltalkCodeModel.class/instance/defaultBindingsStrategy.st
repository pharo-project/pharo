accessing
defaultBindingsStrategy

	^ [ :currentBindings :aSymbol |
		 currentBindings associationAt: aSymbol ifAbsent: [nil] ]