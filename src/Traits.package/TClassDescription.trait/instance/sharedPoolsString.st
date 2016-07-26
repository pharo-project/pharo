printing
sharedPoolsString
	"Answer a string of my shared pool names separated by spaces."

	^String streamContents: [ :stream |
		self sharedPools 
			do: [ :each |
				stream nextPutAll: (self environment 
					keyAtIdentityValue: each 
					ifAbsent: [ 
						each isObsolete
							ifTrue: [ each name ] "obsolete classes should be visible"
							ifFalse: [ 'private' ] ]) ] "if class is from different environment, mark it as private"
			separatedBy: [ stream space ] ]