testing
isGitRoot: aReference
	self flag: #todo. "This does not understand submodules"
	^ aReference isDirectory 
		and: [ (aReference directoriesMatching: '.git') notEmpty
		and: [ (aReference / '.git' / 'config') exists ] ]