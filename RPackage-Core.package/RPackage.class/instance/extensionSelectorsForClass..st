accessing
extensionSelectorsForClass: aClass

	^ aClass isMeta
		ifTrue: [ metaclassExtensionSelectors at: aClass theNonMetaClass originalName ifAbsent: [ #() ] ]
		ifFalse: [ classExtensionSelectors at: aClass originalName ifAbsent: [ #() ] ]