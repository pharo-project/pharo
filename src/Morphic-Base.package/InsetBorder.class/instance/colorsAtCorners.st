accessing
colorsAtCorners
	| c c14 c23 |
	c := self color.
	c14 := c lighter. c23 := c darker.
	^Array with: c23 with: c14 with: c14 with: c23.