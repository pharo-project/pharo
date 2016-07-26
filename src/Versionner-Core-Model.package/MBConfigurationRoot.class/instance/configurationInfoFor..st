accessing
configurationInfoFor: aConfigurationClass

	self configurations do: [:info | info configurationClass == aConfigurationClass theNonMetaClass  ifTrue: [ ^info ]].
	^nil