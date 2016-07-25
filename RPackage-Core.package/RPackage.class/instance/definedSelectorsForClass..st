accessing
definedSelectorsForClass: aClass

	^ aClass isMeta
		ifTrue: [ metaclassDefinedSelectors at: aClass theNonMetaClass originalName ifAbsent: [ #() ] ]
		ifFalse: [ classDefinedSelectors at: aClass originalName ifAbsent: [ #() ] ]