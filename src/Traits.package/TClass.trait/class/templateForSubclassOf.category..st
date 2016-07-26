instance creation
templateForSubclassOf: priorClassName category: systemCategoryName 
	"Answer an expression that can be edited and evaluated in order to define a new class, given that the class previously looked at was as given"

	^priorClassName asString, ' subclass: #NameOfSubclass
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	package: ''' , systemCategoryName asString , ''''