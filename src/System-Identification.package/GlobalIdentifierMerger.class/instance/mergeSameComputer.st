actions
mergeSameComputer
	"It add into existing dictionary keys that are included in the stored dictionary,
	but are missing in the existing dictionary. Useful when a new version
	adds new configuration values that should be saved too.
	
	It returns true if existing dictionary is changed."
	| difference |
	difference := stored keys difference: existing keys.
	difference ifEmpty: [ ^ false ].
	difference do: [ :eachKey |
		existing at: eachKey put: (stored at: eachKey) ].
	self convertValues.
	^ true