other
translateBy:aPoint clippingTo:aRect during:aBlock
	^aBlock value:(self copyOffset:aPoint clipRect:aRect).