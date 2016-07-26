private
lookupPackages
	"Looks into the underlying repository for directories that represent packages."
	^ self repository directory entries select: [ :entry | 
		entry isDirectory and: [ repository canReadFileNamed: entry name ] ].