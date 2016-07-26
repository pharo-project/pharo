private
privateGreen
	"Private! Return the internal representation of my green component."

	^ (rgb bitShift: 0 - GreenShift) bitAnd: ComponentMask