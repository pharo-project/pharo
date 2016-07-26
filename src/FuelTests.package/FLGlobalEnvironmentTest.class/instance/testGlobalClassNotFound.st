tests
testGlobalClassNotFound
	"If a class is not defined in the materialization environment, a proper error should be raised."

	self serialize: Integer.
	
	self should: [ self materialization ] 
		raise: FLClassNotFound 
		description: 'Raise an error since the class is not present in materialization global environment'