private
ensureMetacelloBaseConfiguration
	Smalltalk
		at: #MetacelloBaseConfiguration
		ifAbsent: [ 
			| t1 t2 |
			t2 := MCHttpRepository location: 'http://seaside.gemstone.com/ss/metacello' user: '' password: ''.
			t2
				versionReaderForFileNamed: 'Metacello-Base-DaleHenrichs.2.mcz'
				do: [ :t4 | 
					t1 := t4 version.
					t1 load.
					t1 workingCopy repositoryGroup addRepository: t2 ] ].
	^ self