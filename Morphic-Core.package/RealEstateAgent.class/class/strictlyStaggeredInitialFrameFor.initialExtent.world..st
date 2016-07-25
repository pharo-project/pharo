framing
strictlyStaggeredInitialFrameFor: aStandardSystemView initialExtent: initialExtent world: aWorld
	"This method implements a staggered window placement policy that I (di) like.
	Basically it provides for up to 4 windows, staggered from each of the 4 corners.
	The windows are staggered so that there will always be a corner visible."

	| allowedArea grid initialFrame otherFrames cornerSel corner delta putativeCorner free maxLevel |

	allowedArea :=(self maximumUsableAreaInWorld: aWorld)
		insetBy: (self scrollBarSetback @ self screenTopSetback extent: 0@0).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel := allowedArea area > 300000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid := allowedArea area > 500000 ifTrue: [40] ifFalse: [20].
	initialFrame := 0@0 extent: ((initialExtent
							"min: (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))))
							min: 600@400")).
	otherFrames := (aWorld windowsSatisfying: [:w | w isCollapsed not])
					collect: [:w | w bounds].
	0 to: maxLevel do:
		[:level | 
		1 to: 4 do:
			[:ci | cornerSel := #(topLeft topRight bottomRight bottomLeft) at: ci.
			corner := allowedArea perform: cornerSel.
			"The extra grid//2 in delta helps to keep title tabs distinct"
			delta := (maxLevel-level*grid+(grid//2)) @ (level*grid).
			1 to: ci-1 do: [:i | delta := delta rotateBy: #right centerAt: 0@0]. "slow way"
			putativeCorner := corner + delta.
			free := true.
			otherFrames do:
				[:w |
				free := free & ((w perform: cornerSel) ~= putativeCorner)].
			free ifTrue:
				[^ (initialFrame align: (initialFrame perform: cornerSel)
								with: putativeCorner)
						 translatedAndSquishedToBeWithin: allowedArea]]].
	"If all else fails..."
	^ (self scrollBarSetback @ self screenTopSetback extent: initialFrame extent)
		translatedAndSquishedToBeWithin: allowedArea