removing
unreferencedKeys

	^'Scanning for references . . .' 
		displayProgressFrom: 0 to: self size
		during: 
			[:bar | | n | 
			n := 0.
			self keys select: 
					[:key | 
					bar current: (n := n + 1).
					(self systemNavigation allReferencesTo: (self associationAt: key)) isEmpty]]