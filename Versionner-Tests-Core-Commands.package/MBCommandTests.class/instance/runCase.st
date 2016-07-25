running
runCase
	| original |
	original := MetacelloPlatform current bypassGoferLoadUpdateCategories.
	^ [ 	MetacelloPlatform current bypassGoferLoadUpdateCategories: true.
		[ super runCase ]
			ensure: [ MetacelloPlatform current bypassGoferLoadUpdateCategories: original ] ]