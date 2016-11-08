building
buildClassHierarchyHelpFrom: aRootClass
	| topic |
	topic := HelpTopic named: aRootClass name.
	topic iconName: #classIcon.
	topic contents: aRootClass comment.
	aRootClass
		subclassesDo: [ :subclass | topic addSubtopic: (self buildClassHierarchyHelpFrom: subclass) ].
	^ topic