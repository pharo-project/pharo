utilities
parseMember: fileName
	"Parse a metadata member?"

	"Improvement: try to use the default zip file, since we are retrieving a lot of the metadata info by other means (version, author, timestamp, package name)."

	| monticelloMetaEntry tokens |
	monticelloMetaEntry := packageDirectory , self slash , self monticelloMetaDirName , self slash , fileName.
	tokens := [ self scanner scan: (self zipEntryContents: (self zip memberNamed: monticelloMetaEntry)) ]
		on: Error
		do: [ #() ].
	^ self associate: tokens