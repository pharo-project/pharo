selector handling
removeSelector: aSelector ofMetaclassName: aClassName
	"Remove the method in the package. Note that this method does not remove the method from the class, it just records in the package that the method is not in the package anymore. aClassName is the sole instance class name and not its metaclass one: i.e. adding Point class>>new is done as removeSelector: #new ofMetaclassName: #Point"
	
	self basicRemoveSelector: aSelector ofMetaclassName: aClassName.
	(metaclassExtensionSelectors at: aClassName ifAbsent: [ #() ]) ifEmpty: [
		metaclassExtensionSelectors removeKey: aClassName ifAbsent: [].
	].
	(metaclassDefinedSelectors at: aClassName ifAbsent: [ #() ]) ifEmpty: [
		metaclassDefinedSelectors removeKey: aClassName ifAbsent: [].
	].

	((metaclassExtensionSelectors at: aClassName ifAbsent: [#()]) isEmpty and: [(classExtensionSelectors at: aClassName ifAbsent: [#()]) isEmpty])
		ifTrue: [
				self organizer unregisterExtendingPackage: self forClassName: aClassName
	]