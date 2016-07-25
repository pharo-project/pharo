installation
install
	| sources |
	zip := ZipArchive new.
	zip readFrom: stream.
	self checkDependencies ifFalse: [^false].
	self recordVersionInfo.
	sources := (zip membersMatching: 'snapshot/*') 
				asSortedCollection: [:a :b | a fileName < b fileName].
	sources do: [:src | self installMember: src].