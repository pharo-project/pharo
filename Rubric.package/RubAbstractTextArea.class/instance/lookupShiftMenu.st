menu
lookupShiftMenu
	"default implementation of the algorithm that lookup the menu"

	| default |
	default := [ self editingMode shiftMenu ].
	^ self model 
		ifNil: [ default value ] 
		ifNotNil: [ :m | m shiftMenu ifNil: [ default value ] ]