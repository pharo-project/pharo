accessing
raisedColor
	"Return the color to be used for shading raised borders. The 
	default is my own color, but it might want to be, eg, my 
	owner's color. Whoever's color ends up prevailing, the color 
	itself gets the last chance to determine, so that when, for 
	example, an InfiniteForm serves as the color, callers won't choke 
	on some non-Color object being returned"
	(color isColor
			and: [color isTransparent
					and: [owner notNil]])
		ifTrue: [^ owner raisedColor].
	^ color asColor raisedColor