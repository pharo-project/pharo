!PharoClassInstaller methodsFor: 'migrating' stamp: 'PavelKrivanek 7/19/2016 12:09'!
copyMethodsFrom: oldClass to: newClass using: classModification
	|newClassVariables|
	newClassVariables := classModification classVariables asDictionary.
	oldClass localMethods do: [ :m | | copy |
		copy := m copy.
		copy methodClass: newClass.
		copy literals withIndexDo: [:l :i |
			(l isKindOf: ClassVariable)
				ifTrue: [ | newLiteral |
					newLiteral := classModification newClass innerBindingOf: l key.
					m literalAt: i put: newLiteral ].
		].
		newClass addSelectorSilently: copy selector withMethod: copy.
	].
	oldClass classSide localMethods do: [ :m | | copy |
		copy := m copy.
		copy methodClass: newClass classSide.
		copy literals withIndexDo: [:l :i |
			(l isKindOf: ClassVariable)
				ifTrue: [ | newLiteral |
					newLiteral := classModification newClass innerBindingOf: l key.
					m literalAt: i put: newLiteral ].
		].
		newClass classSide addSelectorSilently: copy selector withMethod: copy.
	].! !



!MCClassDefinition methodsFor: 'installing' stamp: 'MarcusDenker 4/22/2015 19:23'!
createClass
	| superClass |

	name = #Context ifTrue: [^ self].

	superClass := superclassName == #nil ifFalse:
						[ Smalltalk globals at: superclassName ].
	^[PharoClassInstaller make: [ :builder |
		builder
			superclass: superClass;
			name: name;
			layoutClass: (ObjectLayout layoutForType: type);
			slots: self instanceVariables;
			sharedVariables: self classVariables;
			sharedPools: self sharedPoolsString;
			classSlots: self classInstanceVariables;
			traitComposition: self traitCompositionCompiled;
			classTraitComposition: self classTraitCompositionCompiled;
			comment: comment stamp: commentStamp;
			category: category;
			environment:  superClass environment]] on: Warning, DuplicatedVariableError do: [:ex| ex resume].



! !
MonticelloBootstrap bootstrapDefault.
MCCacheRepository reset.
!
