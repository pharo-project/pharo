instance creation
newDirectory
	"Create a new directory within the selected directory."

	|dir dirName title|
	dir := self selectedFileDirectory ifNil: [^self].
	title := 'Create Directory' translated.
	dirName := self
		textEntry: 'Enter directory name' translated
		title: title.
	dirName ifNil: [^self].
	
	[
		(dir / dirName) exists
			ifTrue: [
				^self
					alert: 'A file or directory already exists\with the name' withCRs translated, ' "', dirName, '"'
					title: title
		].
		(dir / dirName) ensureCreateDirectory
	]
	on: Error
	do: [:ex |
		^self
			alert: 'Invalid directory name' translated, ' "', dirName, '"'
			title: title
	].

	self
		clearEntryCache;
		updateDirectories