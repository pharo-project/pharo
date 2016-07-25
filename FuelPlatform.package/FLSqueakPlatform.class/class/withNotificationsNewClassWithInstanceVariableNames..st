private-extensions-classfactoryfortestcase
withNotificationsNewClassWithInstanceVariableNames: instanceVariableNames
	<flExtensionOf: #ClassFactoryForTestCase>
	^ self newSubclassOf: Object instanceVariableNames: instanceVariableNames classVariableNames:  '' category: self defaultCategoryPostfix
	