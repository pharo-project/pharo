converting
asMouseOver
	"Convert the receiver into a mouse over event"
	^MouseEvent basicNew setType: #mouseOver position: position buttons: buttons hand: source