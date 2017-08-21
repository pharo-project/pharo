running
run
	Author uniqueInstance
		ifUnknownAuthorUse: 'hudson'
		during: [ [ 
			self setUp.
			suiteTime := [ self runAll ]
				timeToRun ]
					ensure: [ self tearDown ] ]