private
doLoadRequiredFromArray: anArray

	| displayString oldPolicy oldBypassProgress |
	displayString := self versionNumber printString, ' of ', self spec projectLabel.
	Transcript cr; show: 'Loading ', displayString, '...'.
	oldPolicy := loaderPolicy. 
	oldBypassProgress := MetacelloPlatform current bypassProgressBars.
	self loaderPolicy silently ifTrue: [ MetacelloPlatform current bypassProgressBars: true ].
	[	| fetchLoader |
		fetchLoader := self fetchRequiredFromArray: (self defaultPackageNamesToLoad: anArray).
		MetacelloPlatform current
			do:  [  fetchLoader doLoad ]
			displaying: 'Loading ', displayString.
		Transcript cr; show: '...finished ', self versionNumber printString.
		^fetchLoader ]
		ensure: [ 
			MetacelloPlatform current bypassProgressBars: oldBypassProgress.
			loaderPolicy := oldPolicy ]