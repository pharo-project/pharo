update cycle
drawWorld: aWorld submorphs: submorphs invalidAreasOn: aCanvas 
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that
were redrawn."

	| rectList n morphs rects validList |
	rectList := damageRecorder invalidRectsFullBounds: aWorld viewBox.
	"sort by areas to draw largest portions first"
	rectList := rectList asArray sort: [:r1 :r2 | r1 area > r2 area].
	damageRecorder reset.
	n := submorphs size.
	morphs := OrderedCollection new: n * 2.
	rects := OrderedCollection new: n * 2.
	validList := OrderedCollection new: n * 2.
	rectList do: 
			[:dirtyRect | 
			dirtyRect allAreasOutsideList: validList
				do: 
					[:r | | mm rect i c remnantIntersects remnants rectToFill | 
					"Experimental top-down drawing --
			Traverses top to bottom, stopping if the entire area is filled.
			If only a single rectangle remains, then continue with the reduced rectangle."

					rectToFill := r.
					remnants := OrderedCollection with: r.
					i := 1.
					[remnants isEmpty or: [i > n]] whileFalse: 
							[mm := submorphs at: i.
							((remnantIntersects := remnants select: [:each | (mm fullBounds intersects: each)]) notEmpty and: [mm visible]) 
								ifTrue: 
									[morphs addLast: mm.
									
									rects addLast: (Rectangle merging: (remnantIntersects collect: [:each | mm fullBounds intersect: each])).
									remnants removeAll: remnantIntersects.
									remnantIntersects do: [:eachIntersect | remnants addAll: (mm areasRemainingToFill: eachIntersect)].
									remnants size = 1 ifTrue: [rectToFill := remnants first].
									remnants isEmpty ifTrue: [rectToFill := nil]].
							i := i + 1].
					"Now paint from bottom to top, but using the reduced rectangles."
					rectToFill 
						ifNotNil: [aWorld drawOn: (c := aCanvas copyClipRect: rectToFill)].
					[morphs isEmpty] whileFalse: 
							[(rect := rects removeLast) == rectToFill 
								ifFalse: [c := aCanvas copyClipRect: (rectToFill := rect)].
							c fullDrawMorph: morphs removeLast].
					morphs reset.
					rects reset.
					validList add: r]].
	^validList