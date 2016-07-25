*Versionner-Commit-UI
currentString
	| workingCopy |
	
	workingCopy := self package workingCopy.
	^ [ workingCopy ancestry ancestorStringWithout: workingCopy packageName ]
		on: Error do: [ :e | '' ]