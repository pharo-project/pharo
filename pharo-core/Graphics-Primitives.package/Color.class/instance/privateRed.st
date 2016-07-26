private
privateRed
	"Private! Return the internal representation of my red component."

	^ (rgb bitShift: 0 - RedShift) bitAnd: ComponentMask