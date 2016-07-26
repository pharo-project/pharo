private
methodBindingsStrategy

	^ [ :currentBindings :aSymbol |
		 currentBindings associationAt: aSymbol ifAbsent: [nil] ]