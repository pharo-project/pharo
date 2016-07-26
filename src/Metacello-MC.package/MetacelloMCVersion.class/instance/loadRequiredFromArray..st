private
loadRequiredFromArray: anArray

	| displayString |
	displayString := 'Loading ', self versionNumber printString, ' of ', self spec projectLabel.
	MetacelloPlatform current
		do: [ ^self executeLoadFromArray: anArray ]
		displaying: displayString