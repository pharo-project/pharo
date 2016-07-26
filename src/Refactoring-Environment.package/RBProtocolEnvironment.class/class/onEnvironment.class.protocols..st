instance creation
onEnvironment: anEnvironment class: aClass protocols: aCollection
	^(self onEnvironment: anEnvironment)
		class: aClass protocols: aCollection; yourself