private
addAtStartup: aCollection inDirectory: aFileReference named: fileName

	| scriptFile |
	aFileReference ensureCreateDirectory.
	scriptFile := aFileReference / fileName.
	FileStream forceNewFileNamed: scriptFile fullName do: [ :stream | stream nextPutAll: (self buildStreamFor: aCollection) ].
