halo theme
iconicHaloSpecifications
	"Answer an array that characterizes the locations, colors, icons, and selectors of the halo handles that may be used in the iconic halo scheme"

	"self installHaloTheme: #iconicHaloSpecifications"

	^ #(
	"  	selector				horiz		vert			color info						icon key
		---------				------		-----------		-------------------------------		---------------"
	(addCollapseHandle:		left			topCenter		(tan)							#haloCollapseIcon)
	(addDebugHandle:		right		topCenter		(blue	veryMuchLighter)		#haloDebugIcon)
	(addDismissHandle:		left			top				(red		muchLighter)			#haloDismissIcon)
	(addRotateHandle:		left			bottom			(blue)							#haloRotIcon)
	(addMenuHandle:		leftCenter	top				(red)							#haloMenuIcon)
	(addGrabHandle:			center		top				(black)							#haloGrabIcon)
	(addDragHandle:			rightCenter	top				(brown)							#haloDragIcon)
	(addDupHandle:			right		top				(green)							#haloDupIcon)	
	(addHelpHandle:			center		bottom			(lightBlue)						#haloHelpIcon)
	(addGrowHandle:		right		bottom			(yellow)						#haloScaleIcon)
	(addScaleHandle:		right		bottom			(lightOrange)					#haloScaleIcon)
	(addFontSizeHandle:		leftCenter	bottom			(lightGreen)						#haloFontSizeIcon)
	(addFontStyleHandle:		center		bottom			(lightRed)						#haloFontStyleIcon)
	(addFontEmphHandle:	rightCenter	bottom			(lightBrown darker)				#haloFontEmphIcon)
	(addRecolorHandle:		right		bottomCenter	(magenta darker)				#haloRecolorIcon) )
