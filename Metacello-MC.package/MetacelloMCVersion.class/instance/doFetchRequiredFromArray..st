private
doFetchRequiredFromArray: anArray

	| oldPolicy oldBypassProgress displayString |
	displayString := self versionNumber printString, ' of ', self spec projectLabel.
	Transcript cr; show: 'Fetching ', displayString, '...'.
	oldPolicy := loaderPolicy. 
	oldBypassProgress := MetacelloPlatform current bypassProgressBars.
	self loaderPolicy silently ifTrue: [ MetacelloPlatform current bypassProgressBars: true ].
	[ 	| ans |
		ans := self fetchRequiredFromArray: anArray.
		Transcript cr; show: '...finished ', self versionNumber printString.
		^ans ]
			ensure: [ 
				MetacelloPlatform current bypassProgressBars: oldBypassProgress.
				loaderPolicy := oldPolicy ]