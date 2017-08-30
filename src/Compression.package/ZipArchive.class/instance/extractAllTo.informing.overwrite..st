archive operations
extractAllTo: aDirectory informing: bar overwrite: allOverwrite
	"Extract all elements to the given directory"

	| overwriteAll lastUpdate barValue shouldUpdateInfos |
	overwriteAll := allOverwrite.
	lastUpdate := 0.
	barValue := 0.
	self members
		select: #isDirectory
		thenDo: [ :entry | 
			| dir |
			(shouldUpdateInfos := (Time millisecondsSince: lastUpdate) >= 100)
				ifTrue: [ lastUpdate := Time millisecondClockValue ].
			bar isNotNil & shouldUpdateInfos
				ifTrue: [ bar label: 'Creating ' , entry fileName ].
			dir := (entry fileName findTokens: '/') inject: aDirectory into: [ :base :part | base / part ].
			dir ensureCreateDirectory.
			barValue := barValue + 1.
			bar isNotNil & shouldUpdateInfos
				ifTrue: [ bar value: barValue ] ].
	self members
		select: [ :entry | entry isDirectory not ]
		thenDo: [ :entry | 
			| response |
			(shouldUpdateInfos := (Time millisecondsSince: lastUpdate) >= 100)
				ifTrue: [ lastUpdate := Time millisecondClockValue ].
			bar isNotNil & shouldUpdateInfos
				ifTrue: [ bar label: 'Extracting ' , entry fileName ].
			response := entry extractInDirectory: aDirectory overwrite: overwriteAll.
			response == #retryWithOverwrite
				ifTrue: [ overwriteAll := true.
					response := entry extractInDirectory: aDirectory overwrite: overwriteAll ].
			response == #abort
				ifTrue: [ ^ self ].
			response == #failed
				ifTrue: [ (self confirm: 'Failed to extract ' , entry fileName , '. Proceed?')
						ifFalse: [ ^ self ] ].
			barValue := barValue + 1.
			bar isNotNil & shouldUpdateInfos
				ifTrue: [ bar value: barValue.
					lastUpdate := Time millisecondClockValue ] ]