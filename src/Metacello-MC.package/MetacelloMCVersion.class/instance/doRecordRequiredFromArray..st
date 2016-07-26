private
doRecordRequiredFromArray: anArray

	| originalLoader oldPolicy displayString oldBypassProgress |
	displayString := self versionNumber printString, ' of ', self spec projectLabel.
	Transcript cr; show: 'Recording ', displayString, '...'.
	originalLoader := self versionSpec loader.
	oldPolicy := loaderPolicy. 
	oldBypassProgress := MetacelloPlatform current bypassProgressBars.
	self loaderPolicy silently ifTrue: [ MetacelloPlatform current bypassProgressBars: true ].
	[	MetacelloPlatform current
			do:  [ | ans |
				self versionSpec loader: originalLoader recordingSpecLoader.
				ans := (self executeLoadFromArray: anArray) copy.
				Transcript cr; show: '...finished ', self versionNumber printString.
				^ans ]
			displaying: 'Recording ', displayString ]
				ensure: [
					MetacelloPlatform current bypassProgressBars: oldBypassProgress.
					self versionSpec loader: originalLoader.
					loaderPolicy := oldPolicy ]