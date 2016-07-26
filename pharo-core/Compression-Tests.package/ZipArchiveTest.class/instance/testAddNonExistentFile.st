tests
testAddNonExistentFile
	
	self should: [ zip addFile: 'it_would_be_crazy_if_this_file_existed.ext' ] raise: FileDoesNotExistException.