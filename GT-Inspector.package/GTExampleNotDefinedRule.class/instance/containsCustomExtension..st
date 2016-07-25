running
containsCustomExtension: aClass

	^ aClass methods anySatisfy: [:aMethod | 
		aMethod pragmas anySatisfy: [ :aPragma |
			aPragma keyword = GTInspector extensionsPragma or: [
				aPragma keyword = GTSpotter extensionsPragma ]]]