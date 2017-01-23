accessing
nextStoreName

	| name |
	name := currentStore 
		ifNil: [ "First store name"
			Smalltalk imageFile basenameWithoutExtension, '.1' ]
		ifNotNil: [ "There is already a name"
			currentStore globalName ].
		
	^ self storeNameStrategy nextTo: name in: self directory
