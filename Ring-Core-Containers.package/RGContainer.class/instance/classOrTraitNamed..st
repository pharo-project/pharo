lookup by name
classOrTraitNamed: className
	"A class or metaclass can be reached by its name"
	| theClass |

	className ifNil: [^nil ].
	theClass:= self elementNamed: (self theNonMetaClassNameOf: className) in: self definedClasses.
	theClass ifNil:[ ^nil ].
	
	^(self isMetaclassName: className)
		ifTrue: [ theClass theMetaClass ]
		ifFalse:[ theClass ] 