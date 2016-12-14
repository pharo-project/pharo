registration
registerTrait: aTraitMirror named: aName fullyLoaded: aBoolean

	(environment includesKey: aName)
		ifTrue: [ | oldClass |
			oldClass := environment at: aName.
			oldClass becomeInto: aTraitMirror.
			"oldClassMetaclass target handle == Metaclass
				ifFalse:[ oldClassMetaclass becomeInto: aClassMirror classSide ] "].

	environment at: aName put: aTraitMirror.
	fullyLoaded at: aName put: aBoolean
	"aClassMirror compactClassIndex = 0 ifFalse: [ 
		compactClasses at: aClassMirror compactClassIndex put: aClassMirror ].
	
	objectSpace interpreter resetClassMap.
"