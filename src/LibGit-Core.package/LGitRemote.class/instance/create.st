operations
create
	<todo: 'use exceptions instead of assertions'>
	self assert: url notNil.
	self assert: self repository isReady.
	
	[ self hasFetchSpec
		ifFalse: [ self createWithoutSpec ]
		ifTrue: [ self createWithFetchSpec ] ]
			on: LGit_GIT_EEXISTS
			do: [ "all good then" ]