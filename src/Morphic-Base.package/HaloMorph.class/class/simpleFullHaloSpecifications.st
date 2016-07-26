halo theme
simpleFullHaloSpecifications
	"This method gives the specs for the 'full' handles variant when simple halos are in effect"

	"
	self installHaloTheme: #simpleFullHaloSpecifications
	will result in the standard default halos being reinstalled"
	"NB: listed below in clockwise order"

	^ #(
	"  	selector				horiz		vert			color info						icon key
		---------				------		-----------		-------------------------------		---------------"
	(addDebugHandle:		right		topCenter		(blue	veryMuchLighter)		#haloDebugIcon)
	(addDismissHandle:		left			top				(red		muchLighter)			#haloDismissIcon)
	(addRotateHandle:		left			bottom			(blue)							#haloRotIcon)
	(addMenuHandle:		leftCenter	top				(red)							#haloMenuIcon)
	(addGrabHandle:			center		top				(black)							#halograbIcon)
	(addDragHandle:			rightCenter	top				(brown)							#haloDragIcon)
	(addDupHandle:			right		top				(green)							#haloDupIcon)	
	(addHelpHandle:			center		bottom			(lightBlue)						#haloHelpIcon')
	(addGrowHandle:		right		bottom			(yellow)						#haloScaleIcon)
	(addScaleHandle:		right		bottom			(lightOrange)					#haloScaleIcon)
	(addFewerHandlesHandle:	left		topCenter		(paleBuff)						#haloFewerHandlesIcon')
	(addFontSizeHandle:		leftCenter	bottom			(lightGreen)						#haloFontSizeIcon)
	(addFontStyleHandle:		center		bottom			(lightRed)						#haloFontStyleIcon)
	(addFontEmphHandle:	rightCenter	bottom			(lightBrown darker)		  		#haloFontEmphIcon)
	(addRecolorHandle:		right		bottomCenter	(magenta darker)				#haloRecolorIcon) ) 