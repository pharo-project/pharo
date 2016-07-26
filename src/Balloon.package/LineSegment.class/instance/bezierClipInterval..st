bezier clipping
bezierClipInterval: aCurve
	"Compute the new bezier clip interval for the argument,
	based on the fat line (the direction aligned bounding box) of the receiver.
	Note: This could be modified so that multiple clip intervals are returned.
	The idea is that for a distance curve like

			x		x
	tMax----	--\-----/---\-------
				x		x
	tMin-------------------------

	all the intersections intervals with tMin/tMax are reported, therefore
	minimizing the iteration count. As it is, the process will slowly iterate
	against tMax and then the curve will be split.
	"
	| nrm tStep pts eps inside tValue tMin tMax last lastV lastT lastInside next nextV nextT nextInside vMin vMax |
	eps := 0.00001.					"distance epsilon"
	nrm := (start y - end y) @ (end x - start x). "normal direction for (end-start)"

	"Map receiver's control point into fat line; compute vMin and vMax"
	vMin := vMax := nil.
	self controlPointsDo:[:pt| | vValue |
		vValue := (nrm x * pt x) + (nrm y * pt y). "nrm dotProduct: pt."
		vMin == nil	ifTrue:[	vMin := vMax := vValue]
					ifFalse:[vValue < vMin ifTrue:[vMin := vValue].
							vValue > vMax ifTrue:[vMax := vValue]]].
	"Map the argument into fat line; compute tMin, tMax for clip"
	tStep := 1.0 / aCurve degree.
	pts := aCurve controlPoints.
	last := pts at: pts size.
	lastV := (nrm x * last x) + (nrm y * last y). "nrm dotProduct: last."
	lastT := 1.0.
	lastInside := lastV+eps < vMin ifTrue:[-1] ifFalse:[lastV-eps > vMax ifTrue:[1] ifFalse:[0]].

	"Now compute new minimal and maximal clip boundaries"
	inside := false.	"assume we're completely outside"
	tMin := 2.0. tMax := -1.0. 	"clip interval"
	1 to: pts size do:[:i|
		next := pts at: i.
		nextV := (nrm x * next x) + (nrm y * next y). "nrm dotProduct: next."
		false ifTrue:[
			(nextV - vMin / (vMax - vMin)) printString displayAt: 0@ (i-1*20)].
		nextT := i-1 * tStep.
		nextInside := nextV+eps < vMin ifTrue:[-1] ifFalse:[nextV-eps > vMax ifTrue:[1] ifFalse:[0]].
		nextInside = 0 ifTrue:[
			inside := true.
			tValue := nextT.
			tValue < tMin ifTrue:[tMin := tValue].
			tValue > tMax ifTrue:[tMax := tValue].
		].
		lastInside = nextInside ifFalse:["At least one clip boundary"
			inside := true.
			"See if one is below vMin"
			(lastInside + nextInside <= 0) ifTrue:[
				tValue := lastT + ((nextT - lastT) * (vMin - lastV) / (nextV - lastV)).
				tValue < tMin ifTrue:[tMin := tValue].
				tValue > tMax ifTrue:[tMax := tValue].
			].
			"See if one is above vMax"
			(lastInside + nextInside >= 0) ifTrue:[
				tValue := lastT + ((nextT - lastT) * (vMax - lastV) / (nextV - lastV)).
				tValue < tMin ifTrue:[tMin := tValue].
				tValue > tMax ifTrue:[tMax := tValue].
			].
		].
		last := next.
		lastT := nextT.
		lastV := nextV.
		lastInside := nextInside.
	].
	inside
		ifTrue:[^Array with: tMin with: tMax]
		ifFalse:[^nil]