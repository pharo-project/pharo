converting
asUndoOperation
	| trait |
	trait := Smalltalk globals
		at: self changeClassName
		ifAbsent: [ nil ].
	^ trait isTrait
		ifTrue: [ RBRefactoryChangeManager changeFactory addTraitDefinition: trait definition ]
		ifFalse: [ RBRefactoryChangeManager changeFactory removeClassNamed: self changeClassName ]