private
createSubclassFrom: superClass named: symbol
	| newClass |
	
	newClass := superClass 
					subclass: symbol
					instanceVariableNames: ''
					classVariableNames: ''
					poolDictionaries: ''
					category: 'SmartSuggestions-Test'.
	^newClass.