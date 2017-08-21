converting
asUndoOperation
	| class |
	class := Smalltalk globals
		at: self changeClassName
		ifAbsent: [ nil ].
	^ class isBehavior
		ifTrue: [ RBRefactoryChangeManager changeFactory addClassDefinition: class definition ]
		ifFalse: [ RBRefactoryChangeManager changeFactory removeClassNamed: self changeClassName ]