temp vars
addCopyingTempToAllScopesUpToDefTemp: aVar

	(self hasCopyingTempNamed: aVar name) ifFalse: [self addCopyingTemp: aVar].
	tempVars at: aVar name ifPresent: [:v | ^ self].
	^ self outerScope addCopyingTempToAllScopesUpToDefTemp: aVar.
				