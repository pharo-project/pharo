accessing
textStyle

	^ TextStyle actualTextStyles detect: [:aStyle | (aStyle fontArray collect: [:s | s name]) includes: self name]
		ifNone: [].
