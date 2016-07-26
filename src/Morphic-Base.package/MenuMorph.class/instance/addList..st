construction
addList: aList
	"Add the given items to this menu, where each item is a pair (<string> <actionSelector>)..  If an element of the list is simply the symobl $-, add a line to the receiver.  The optional third element of each entry, if present, provides balloon help. The optional fourth element provide the icon selector"

	aList
		do: [ :tuple | 
			tuple == #-
				ifTrue: [ self addLine ]
				ifFalse: [ 
					self add: tuple first capitalized translated selector: tuple second.
					(tuple size > 2 and: [ tuple third notNil ])
						ifTrue: [ self balloonTextForLastItem: tuple third translated ].
					(tuple size > 3 and: [ tuple fourth notNil ])
						ifTrue: [ self lastItem icon: (Smalltalk ui icons iconNamed: tuple fourth) ] ] ]