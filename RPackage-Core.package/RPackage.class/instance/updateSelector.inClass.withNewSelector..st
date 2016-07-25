updating
updateSelector: oldSelector inClass: aClass withNewSelector: newSelector

	"here we check if the old seletor is defined in the package or if it is an extension. Then we dispatch the work to the corresponding method.
	For one specific class, a selector is either a defined selector or an extension selector, there is no way to have a couple of class->selector present in both dictionary"
	
	(self includesDefinedSelector: oldSelector ofClass: aClass) 
		ifTrue: [^ self updateDefinedSelector: oldSelector inClass: aClass  withNewSelector: newSelector].
	(self includesExtensionSelector: oldSelector ofClass: aClass)
		ifTrue: [^ self updateExtensionSelector: oldSelector inClass: aClass  withNewSelector: newSelector].
		
		