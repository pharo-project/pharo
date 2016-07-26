halo theme
customHaloSpecifications
	"Intended for you to modify to suit your personal preference.  What is implemented in the default here is just a skeleton; in comment at the bottom of this method are some useful lines you may wish to paste in to the main body here, possibly modifying positions, colors, etc..
	Note that in this example, we include:
			Dismiss handle, at top-left
			Menu handle, at top-right
			Resize handle, at bottom-right
			Rotate handle, at bottom-left
			Drag handle, at top-center
			Recolor handle, at left-center.  (this one is NOT part of the standard formulary --
											it is included here to illustrate how to
 											add non-standard halos)
			Note that the optional handles for specialized morphs, such as Sketch, Text, PasteUp, are also included"

	^ #(
	(addDismissHandle:		left			top				(red		muchLighter)			#haloDismissIcon)
	(addMenuHandle:		right		top				(red)							#haloMenuIcon)
	(addDragHandle:			center	top					(brown)							#haloDragIcon)
	(addGrowHandle:		right		bottom			(yellow)						#haloScaleIcon)
	(addScaleHandle:		right		bottom			(lightOrange)					#haloScaleIcon)

	(addRecolorHandle:		left			center			(green muchLighter lighter)		#haloRecolorIcon)
	(addFontSizeHandle:		leftCenter	bottom			(lightGreen)						#haloFontSizeIcon)
	(addFontStyleHandle:		center		bottom			(lightRed)						#haloFontStyleIcon)
	(addFontEmphHandle:	rightCenter	bottom			(lightBrown darker)				#haloFontEmphIcon)
	(addRotateHandle:		left			bottom			(blue)							#haloRotIcon)

	(addDebugHandle:		right		topCenter		(blue	veryMuchLighter)		#haloDebugIcon) )

	"  Other useful handles...

  		selector				horiz		vert			color info						icon key
		---------				------		-----------		-------------------------------		---------------

	(addTileHandle:			left			bottomCenter	(lightBrown)					#haloTileIcon)
	(addViewHandle:			left			center			(cyan)							#haloViewIcon)
	(addGrabHandle:			center		top				(black)							#haloGrabIcon)
	(addDragHandle:			rightCenter	top				(brown)							#haloDragIcon)
	(addDupHandle:			right		top				(green)							#haloDupIcon)	
	(addHelpHandle:			center		bottom			(lightBlue)						#haloHelpIcon)
	(addFewerHandlesHandle:	left		topCenter		(paleBuff)						#haloFewerHandlesIcon)
	(addPaintBgdHandle:		right		center			(lightGray)						#haloPaintIcon)
	"
