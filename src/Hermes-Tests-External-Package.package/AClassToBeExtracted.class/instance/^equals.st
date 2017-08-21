comparing
= aComparable 
	(self species ~= aComparable species) ifTrue:[^false].
	^ self aVariable = aComparable aVariable.