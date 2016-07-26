archive operations
extractAllTo: aDirectory informing: bar overwrite: allOverwrite
	"Extract all elements to the given directory"
	| overwriteAll |
	overwriteAll := allOverwrite.
	self members do:[:entry| | dir |
		entry isDirectory ifTrue:[
			bar ifNotNil: [bar label: 'Creating ', entry fileName].
			dir := (entry fileName findTokens:'/') 
					inject: aDirectory into:[:base :part| base / part].
			dir ensureCreateDirectory.
		].
	].
	self members do:[:entry| | response |
		entry isDirectory ifFalse:[
			bar ifNotNil: [bar label: 'Extracting ', entry fileName].
			response := entry extractInDirectory: aDirectory overwrite: overwriteAll.
			response == #retryWithOverwrite ifTrue:[
				overwriteAll := true.
				response := entry extractInDirectory: aDirectory overwrite: overwriteAll.
			].
			response == #abort ifTrue:[^self].
			response == #failed ifTrue:[
				(self confirm: 'Failed to extract ', entry fileName, '. Proceed?') ifFalse:[^self].
			].
		].
	].
