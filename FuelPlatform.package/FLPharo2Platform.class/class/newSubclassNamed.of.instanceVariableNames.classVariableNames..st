private-extensions-flserializationtest
newSubclassNamed: nameString of: aClass  instanceVariableNames: ivNamesString classVariableNames: classVarsString
	| class |
	<flExtensionOf: #FLSerializationTest>
	 	SystemAnnouncer uniqueInstance suspendAllWhile: [
		class := aClass
			subclass: nameString asSymbol
			instanceVariableNames: ivNamesString
			classVariableNames:  classVarsString
			poolDictionaries: ''
			category: self classFactory defaultCategory
	].
	self classFactory createdClasses add: class.
	^ class