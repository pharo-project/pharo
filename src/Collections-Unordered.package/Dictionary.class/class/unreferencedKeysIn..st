private
unreferencedKeysIn: aDictionary
	^ 'Scanning for references . . .'
		displayProgressFrom: 0
		to: aDictionary size
		during: [ :progressBar | 
			| n |
			n := 0.
			aDictionary keys
				select: [ :key | 
					progressBar current: (n := n + 1).
					(self systemNavigation
						allReferencesTo: (aDictionary associationAt: key)) isEmpty ] ]