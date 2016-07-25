converting
asUndoOperation
	| class |
	class := Smalltalk globals
		at: self changeClassName
		ifAbsent: [ nil ].
	^ class isBehavior
		ifTrue: [ RBAddClassChange definition: class definition ]
		ifFalse: [ RBRemoveClassChange removeClassName: self changeClassName ]