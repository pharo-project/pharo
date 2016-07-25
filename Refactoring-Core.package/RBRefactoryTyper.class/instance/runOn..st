accessing
runOn: aClass 
	variableTypes := Dictionary new.
	variableMessages := Dictionary new.
	bestGuesses := Dictionary new.
	class := model classFor: aClass.
	class instanceVariableNames isEmpty ifTrue: [^self].
	self
		selectedClass: aClass;
		computeEquivalenceClassesForMethodsAndVars;
		computeMessagesSentToVariables;
		computeTypes;
		refineTypesByLookingAtAssignments