private-tab label
modifyClippingBounds: clippingBounds
	| rightOffset |

	rightOffset := -10.
	closeIcon visible ifTrue: [ rightOffset := rightOffset - 14 ].

	^ clippingBounds withRight:clippingBounds right + rightOffset