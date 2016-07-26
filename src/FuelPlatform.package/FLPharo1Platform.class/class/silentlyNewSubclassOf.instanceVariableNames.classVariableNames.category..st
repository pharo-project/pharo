private-extensions-classfactoryfortestcase
silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames:  classVarsString category: category
	<flExtensionOf: #ClassFactoryForTestCase>
	| newClass |
	[ newClass := aClass 
			subclass: self newName 
			instanceVariableNames: ivNamesString 
			classVariableNames: classVarsString 
			poolDictionaries: '' 
			category: (self packageName, '-', category) asSymbol.
	] fuelValueWithoutNotifications.

	self createdClasses add: newClass.
	^newClass
	