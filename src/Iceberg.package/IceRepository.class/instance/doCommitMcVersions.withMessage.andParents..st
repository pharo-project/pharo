private
doCommitMcVersions: versions withMessage: message andParents: parentCommitishList
	self addFilesToIndex: (versions collect: [ :version | self directoryForPackage: version package ]).
	^ self commitIndexWithMessage: message andParents: parentCommitishList