operations
packHead
	<todo: 'This method is distructive. It will delete *all* objects, including those not in the pack (not reachable from head'>
	"USE VERY CAREFULLY"
	
	LGitGlobal runSequence: [ self createPackForReference: self head ].
	self deleteAllLooseObjectsForReference: self head