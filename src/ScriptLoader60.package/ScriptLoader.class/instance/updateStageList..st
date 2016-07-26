elementary steps
updateStageList: aString
	| stagedFile fileExists |
	"Writes the pending updates into a 'staging' list, so the continuous integration can 
	 process and move it to updates when validated (and not before)."
	
	stagedFile := self stagedListFile.
	fileExists := stagedFile exists.
	stagedFile writeStreamDo: [ :stream |
		fileExists 
			ifTrue: [ stream setToEnd ]
			ifFalse: [ stream nextPut: $#; nextPutAll: SystemVersion current version asString ].
		stream
			cr;
			nextPutAll: self currentUpdateVersionNumber asString;
			nextPutAll: '-Pha-', aString, '.cs' ]