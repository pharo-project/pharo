halo theme
classicHaloSpecifications
	"Non-iconic halos with traditional placements"

	"
	self installHaloTheme: #classicHaloSpecifications
	will result in the standard default halos being reinstalled"
	"NB: listed below in clockwise order"

		^ #(
	"  	selector				horiz		vert			color info						icon key
		---------				------		-----------		-------------------------------		---------------"
	(addMenuHandle:		left			top				(red)							none)
	(addDismissHandle:		leftCenter	top				(red		muchLighter)			#haloDismissIcon)
	(addGrabHandle:			center		top				(black)							none)
	(addDragHandle:			rightCenter	top				(brown)							none)
	(addDupHandle:			right		top				(green)							none)	
	(addDebugHandle:		right		topCenter		(blue	veryMuchLighter)		none)
	(addGrowHandle:		right		bottom			(yellow)						none)
	(addScaleHandle:		right		bottom			(lightOrange)					none)
	(addFontEmphHandle:	rightCenter	bottom			(lightBrown darker)				none)
	(addFontStyleHandle:		center		bottom			(lightRed)						none)
	(addFontSizeHandle:		leftCenter	bottom			(lightGreen)						none)

	(addRecolorHandle:		right		bottomCenter	(magenta darker)				none)

	(addRotateHandle:		left			bottom			(blue)							none))

