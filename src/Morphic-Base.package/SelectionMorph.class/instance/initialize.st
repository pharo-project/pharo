initialization
initialize
	"initialize the state of the receiver"
	super initialize.
	
	selectedItems := OrderedCollection new.
	itemsAlreadySelected := OrderedCollection new.
	slippage := 0@0