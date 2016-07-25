instance creation
class: aClass protocols: aCollection
	^ self 
		onEnvironment: self default
		class: aClass
		protocols: aCollection