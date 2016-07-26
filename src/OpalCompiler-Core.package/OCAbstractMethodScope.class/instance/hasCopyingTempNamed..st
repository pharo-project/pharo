temp vars
hasCopyingTempNamed: name
	^self copiedVars anySatisfy: [:each | each name = name]