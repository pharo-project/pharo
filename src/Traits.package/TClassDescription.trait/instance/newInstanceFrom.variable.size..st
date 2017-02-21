private
newInstanceFrom: oldInstance variable: variable size: instSize
	"Create a new instance of the receiver based on the given old instance.
	The supplied map contains a mapping of the old instVar names into
	the receiver's instVars"

	| new  value |
	variable
		ifTrue: [ new := self basicNew: oldInstance basicSize ]
		ifFalse: [ new := self basicNew ].
		
	"Slot migration happens there"	
	self allSlots
		do: [ :newSlot | 
			oldInstance class
				slotNamed: newSlot name
				ifFound: [ :oldSlot | 
					newSlot wantsInitalization ifTrue: [ self initializeSlots: new ].
					value := oldSlot read: oldInstance.
					newSlot write: value to: new ] ].
	variable
		ifTrue: [ 1 to: oldInstance basicSize do: [ :offset | new basicAt: offset put: (oldInstance basicAt: offset) ] ].
	^ new