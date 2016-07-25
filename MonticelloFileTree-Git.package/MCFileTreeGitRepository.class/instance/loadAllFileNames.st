accessing
loadAllFileNames
	"When reading the file names, generate GitFileTreePackageEntry objects with the path and the info."

	"Algorithm: may move in a dedicated scanner/parser in the future:
	input :
		line *;
	line :
		log_size log_data ;
	log_size :
		'log size' <integer> ;
	log_data :
		<SHA1> '|' user_name '|' time_stamp '|' message ;
	End of message can be obtained by the log_size entry.
	"

	| allNames entries |
	allNames := OrderedCollection new.
	entries := self directory entries select: [ :entry | entry isDirectory and: [ self canReadFileNamed: entry name ] ].
	'loading packages versions from git'
		displayProgressFrom: 0
		to: entries size
		during: [ :bar | 
			entries
				doWithIndex: [ :entry :index | 
					| entryVersions |
					entryVersions := self getVersionsForPackageDirectoryEntry: entry.
					allNames addAll: entryVersions.
					bar value: index ] ].
	^ allNames