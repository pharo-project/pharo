converting
asUndoOperation
	| trait |
	trait := Smalltalk globals
		at: self changeClassName
		ifAbsent: [ nil ].
	^ trait isTrait
		ifTrue: [ RBAddTraitChange definition: trait definition ]
		ifFalse: [ RBRemoveClassChange removeClassName: self changeClassName ]