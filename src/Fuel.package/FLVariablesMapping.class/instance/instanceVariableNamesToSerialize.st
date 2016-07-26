private
instanceVariableNamesToSerialize

	| ignoredInstanceVariableNames instanceVariableNamesToSerialize |
	ignoredInstanceVariableNames := theClass fuelIgnoredInstanceVariableNames.
	instanceVariableNamesToSerialize := OrderedCollection new.

	theClass instanceVariableNamesDo: [:name | 
		(ignoredInstanceVariableNames includes: name) 
			ifFalse: [ instanceVariableNamesToSerialize add: name ]].

	^ instanceVariableNamesToSerialize