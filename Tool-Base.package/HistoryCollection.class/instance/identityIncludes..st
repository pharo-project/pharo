protocol
identityIncludes: anElement

	^ storage anySatisfy: [:association | association key == anElement ]