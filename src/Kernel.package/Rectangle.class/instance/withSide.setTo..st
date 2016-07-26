rectangle functions
withSide: side setTo: value  "return a copy with side set to value"
	^ self perform: (#(withLeft: withRight: withTop: withBottom: )
							at: (#(left right top bottom) indexOf: side))
		with: value