loading
loadIconsFromUrl
	| newIcons zipArchive |
	
	newIcons := IdentityDictionary new.
	zipArchive := self downloadFromUrl.
	((FileSystem zip: zipArchive) open workingDirectory allChildrenMatching: '*.png')
		reject: [ :each | each base beginsWith: '.' ]
		thenDo: [ :each | 
			[ newIcons 	
				at: each base asSymbol
				put: (self readPNGFrom: each) ]
			on: Error do: [ :e | self crLog: each fullName, ' not a PNG, skipping.'  ]].
	icons := newIcons.