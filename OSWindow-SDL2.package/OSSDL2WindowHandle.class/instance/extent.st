accessing
extent
	| w h |
	w := ByteArray new: 4.
	h := ByteArray new: 4.
	handle getSizeW: w h: h.
	^ (w signedLongAt: 1) @ (h signedLongAt: 1)