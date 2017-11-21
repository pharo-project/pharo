Class {
	#name : #VSValidateConfigurationCommandTest,
	#superclass : #MBCommandTests,
	#category : #Versionner-Tests-Core-Commands
}

{ #category : #tests }
VSValidateConfigurationCommandTest >> testExecute [
	self
		should: [ (VSValidateConfigurationCommand target: (MBConfigurationInfo configurationClass: configuration) for: nil) execute ]
		raise: MetacelloStackCacheNotification
]
