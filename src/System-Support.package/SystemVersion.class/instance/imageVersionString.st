accessing - version strings
imageVersionString
	"Print the version compatibile with naming the image."
	
	^String streamContents: [:str |
		str nextPutAll: self type;
			 nextPut: $-;
			 nextPutAll: (self majorMinor: '.').
			
		self hasMetadataIdentifiers ifTrue: [ str nextPut: $+ ].		
		self suffix ifNotEmpty: [ str nextPutAll: self suffix ].
		self hasValidBuildNumber ifTrue: [ 
			self suffix ifNotEmpty: [ str nextPut: $.].
			str nextPutAll: 'build.'; nextPutAll: self build asString ].	
		self commitHash ifNotEmpty: [ str nextPutAll: '.sha.'; nextPutAll: self commitHash ].
		
		Smalltalk vm is32bit ifTrue: [ str nextPutAll: ' (32 Bit)' ]
								   ifFalse: [ str nextPutAll: ' (64 Bit)' ].					
	]