libgit-calls
clone: aString options: cloneOptions to: aFileReference
	<todo: 'use exceptions instead of assertion'>
	self assert: aString isEmptyOrNil not.
	self assert: aFileReference notNil.
	self assert: cloneOptions notNil.
	
	aFileReference ensureCreateDirectory.
	
	self withReturnHandlerDo: [ 
		self 
			clone: self
			url: aString
			local_path: aFileReference pathString
			options: cloneOptions ]