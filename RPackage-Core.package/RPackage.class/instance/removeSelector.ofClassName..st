selector handling
removeSelector: aSelector ofClassName: aClassName
	"Remove the method in the package but does not propagate to the class itself.  Note that this method does not remove the method from the class, it just records in the package that the method is not in the package anymore."
	
	self basicRemoveSelector: aSelector ofClassName: aClassName.
	(classExtensionSelectors at: aClassName ifAbsent: [ #() ]) ifEmpty: [
		classExtensionSelectors removeKey: aClassName ifAbsent: [].
	].
	(classDefinedSelectors at: aClassName ifAbsent: [ #() ]) ifEmpty: [
		classDefinedSelectors removeKey: aClassName ifAbsent: [].
	].

	((metaclassExtensionSelectors at: aClassName ifAbsent: [#()]) isEmpty and: [(classExtensionSelectors at: aClassName ifAbsent: [#()]) isEmpty])
		ifTrue: [
				self organizer unregisterExtendingPackage: self forClassName: aClassName
	]