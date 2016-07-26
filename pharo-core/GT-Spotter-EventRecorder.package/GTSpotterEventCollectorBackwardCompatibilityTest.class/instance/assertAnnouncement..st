asserting
assertAnnouncement: announcement
	self assert: announcement notNil.
	self assert: (announcement isKindOf: GTEventAnnouncement).
	announcement unpackedData do: [ :eachEvent |
		"Check that all instance variables have some value."
		1 to: eachEvent class instSize do: [ :index |
			self 
				assert: (eachEvent instVarAt: index) notNil 
				description: [ String streamContents: [ :aStream |
						aStream 
							nextPutAll: 'Instance variable called ';
							print: (eachEvent class instVarNameForIndex: index);
							nextPutAll: ' of instance ';
							nextPutAll: eachEvent className;
							nextPutAll: ' is nil.' ] ]
		]
	]