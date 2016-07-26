running
runCase
	| original |
	original := MetacelloPlatform current bypassGoferLoadUpdateCategories.
	[ 
	MetacelloPlatform current bypassGoferLoadUpdateCategories: true.
	^ MetacelloPlatform current suspendSystemUpdateEventsDuring: [ super runCase ] ]
		ensure: [ MetacelloPlatform current bypassGoferLoadUpdateCategories: original ]