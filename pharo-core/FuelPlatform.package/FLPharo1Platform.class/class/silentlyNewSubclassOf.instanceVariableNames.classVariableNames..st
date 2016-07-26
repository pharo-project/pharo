private-extensions-classfactoryfortestcase
silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames:  classVarsString
	<flExtensionOf: #ClassFactoryForTestCase>
	^ self silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames:  classVarsString category: self defaultCategoryPostfix
