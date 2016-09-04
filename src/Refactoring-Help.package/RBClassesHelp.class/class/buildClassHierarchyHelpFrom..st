building
buildClassHierarchyHelpFrom: aRootClass
	| topic |
	topic := HelpTopic named: aRootClass name.
	topic icon: (self iconNamed: #classIcon).
	topic contents: aRootClass comment.
	aRootClass
		subclassesDo: [ :subclass | topic addSubtopic: (self buildClassHierarchyHelpFrom: subclass) ].
	^ topic