private-extensions-classfactoryfortestcase
silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames:  classVarsString package: packageName
	<flExtensionOf: #ClassFactoryForTestCase>
	| newClass |
	[ newClass := aClass 
			subclass: self newName 
			instanceVariableNames: ivNamesString 
			classVariableNames: classVarsString 
			poolDictionaries: '' 
			category: packageName asSymbol.
	] fuelValueWithoutNotifications.

	self createdClasses add: newClass.
	^newClass
	