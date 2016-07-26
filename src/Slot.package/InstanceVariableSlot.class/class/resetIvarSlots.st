cleanup
resetIvarSlots
	"when the Ivar slots need to be re-created (e.g. due to changes in the layout of the class),
	 this method re-creates for every ivar a ivar slot"
	"to be removed later when the system is in a stable state"

	| block |
	block := [ :class | 
	class instVarNames
		withIndexDo: [ :ivarName :index | 
			class classLayout slotScope
				at: index
				put:
					((InstanceVariableSlot named: ivarName asSymbol)
						index: index + class superclass allInstVarNames size;
						yourself) ] ].
	Smalltalk allClasses
		do: [ :class | 
			block value: class.
			block value: class classSide ]