private
informValidationError: anError
	UIManager default inform: (String streamContents: [ :stream |
		stream 
			<< anError project name << ' has errors:'; cr. 
		anError issues 
			do: [ :each | stream << each ]
			separatedBy: [ stream cr ] ])