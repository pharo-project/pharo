system integration
systemMethodRemovedActionFrom: ann

	| method methodPackage |
	method := ann method.
	"If the method is provided by a trait, we do not care about it"
	ann isProvidedByATrait ifTrue: [^ self].

	ann methodClass isMeta
		ifFalse: [ 
			methodPackage := self packageDefiningOrExtendingSelector: ann selector inClassNamed: ann methodClass theNonMetaClass originalName.
			methodPackage ifNotNil: [ 
				methodPackage removeSelector: ann selector ofClassName: ann methodClass theNonMetaClass originalName].
		]
		ifTrue: [ 
			methodPackage := self packageDefiningOrExtendingSelector: ann selector inMetaclassNamed: ann methodClass theNonMetaClass originalName.
			methodPackage ifNotNil: [ 
				methodPackage removeSelector: ann selector ofMetaclassName: ann methodClass theNonMetaClass originalName].
		]