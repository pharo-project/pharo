private
addHandle: handleSpec on: eventName send: selector to: recipient 
	"Add a handle within the halo box as per the haloSpec, and set 
	it up to respond to the given event by sending the given 
	selector to the given recipient. Return the handle."
	| handle aPoint |
	aPoint := self
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	handle := self
				addHandleAt: aPoint
				color: (Color colorFrom: handleSpec color)
				icon: handleSpec iconSymbol
				on: eventName
				send: selector
				to: recipient.
	^ handle