help
help
	<wikiStyleHelp: 'RPackage'>	
	|stream|
	stream := String new writeStream.
	stream 
		nextPut: $!;
		nextPutAll: self name asString;
		cr;
		nextPutAll: self comment;
		cr;
		nextPut: $!;
		nextPutAll: self packageOrganizerClass name asString;
		cr;
		nextPutAll: self packageOrganizerClass comment.		
	^stream contents
	
	