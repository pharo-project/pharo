protocol
includes: anElement

	^ storage anySatisfy: [:association | association key = anElement ]