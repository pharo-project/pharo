private
clipRange
	"clip and adjust source origin and extent appropriately"
	"first in x"
	"fill in the lazy state if needed"
	| sx sy dx dy bbW bbH |
	destX ifNil: [ destX := 0 ].
	destY ifNil: [ destY := 0 ].
	width ifNil: [ width := destForm width ].
	height ifNil: [ height := destForm height ].
	sourceX ifNil: [ sourceX := 0 ].
	sourceY ifNil: [ sourceY := 0 ].
	clipX ifNil: [ clipX := 0 ].
	clipY ifNil: [ clipY := 0 ].
	clipWidth ifNil: [ clipWidth := destForm width ].
	clipHeight ifNil: [ clipHeight := destForm height ].
	destX >= clipX 
		ifTrue: 
			[ sx := sourceX.
			dx := destX.
			bbW := width ]
		ifFalse: 
			[ sx := sourceX + (clipX - destX).
			bbW := width - (clipX - destX).
			dx := clipX ].
	dx + bbW > (clipX + clipWidth) ifTrue: [ bbW := bbW - (dx + bbW - (clipX + clipWidth)) ].
	"then in y"
	destY >= clipY 
		ifTrue: 
			[ sy := sourceY.
			dy := destY.
			bbH := height ]
		ifFalse: 
			[ sy := sourceY + clipY - destY.
			bbH := height - (clipY - destY).
			dy := clipY ].
	dy + bbH > (clipY + clipHeight) ifTrue: [ bbH := bbH - (dy + bbH - (clipY + clipHeight)) ].
	sourceForm ifNotNil: 
		[ sx < 0 ifTrue: 
			[ dx := dx - sx.
			bbW := bbW + sx.
			sx := 0 ].
		sx + bbW > sourceForm width ifTrue: [ bbW := bbW - (sx + bbW - sourceForm width) ].
		sy < 0 ifTrue: 
			[ dy := dy - sy.
			bbH := bbH + sy.
			sy := 0 ].
		sy + bbH > sourceForm height ifTrue: [ bbH := bbH - (sy + bbH - sourceForm height) ] ].
	(bbW <= 0 or: [ bbH <= 0 ]) ifTrue: 
		[ sourceX := sourceY := destX := destY := clipX := clipY := width := height := 0.
		^ true ].
	(sx = sourceX and: 
		[ sy = sourceY and: 
			[ dx = destX and: [ dy = destY and: [ bbW = width and: [ bbH = height ] ] ] ] ]) ifTrue: [ ^ false ].
	sourceX := sx.
	sourceY := sy.
	destX := dx.
	destY := dy.
	width := bbW.
	height := bbH.
	^ true