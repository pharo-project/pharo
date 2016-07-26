modal control
invokeModal
	"Invoke this menu and don't return until the user has chosen a value.
	See example below on how to use modal menu morphs."
	^ self invokeModal: self menuKeyboardControl

	"Example:
	| menu sub entry |
	menu := MenuMorph new.
	1 to: 3 do: [:i |
		entry := 'Line', i printString.
		sub := MenuMorph new.
		menu add: entry subMenu: sub.
		#('Item A' 'Item B' 'Item C')  do:[:subEntry|
			sub add: subEntry target: menu 
				selector: #modalSelection: argument: {entry. subEntry}]].
	menu invokeModal.
"

