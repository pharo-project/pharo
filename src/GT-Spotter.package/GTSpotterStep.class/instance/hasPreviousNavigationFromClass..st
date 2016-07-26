testing
hasPreviousNavigationFromClass: aClass
	"Return true if origin of the previous step is kind of aClass.
	Return false if there is no previous step or if origin is not a kind of aClass"
	<return: #Boolean>
	
	^ self hasPreviousStep and: [
		self previousStep origin isKindOf: aClass ]