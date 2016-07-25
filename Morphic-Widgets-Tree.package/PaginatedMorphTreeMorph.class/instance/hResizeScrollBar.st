geometry
hResizeScrollBar
	super hResizeScrollBar.
	hScrollBar bounds: ((hScrollBar bounds withTop: (hScrollBar top - self pagerHeight)) withBottom: (hScrollBar bottom - self pagerHeight))