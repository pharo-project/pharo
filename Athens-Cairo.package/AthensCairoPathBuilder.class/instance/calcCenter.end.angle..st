private
calcCenter: start end: end angle: angle

	| v  rot  center radius len sina cosa m |	

	v := end - start.
	
	m := AthensAffineTransform new rotateByRadians: (Float pi - angle   /2).

	v := m transform: v.		
	len := v r.
	radius := len / 2 / (angle /2) sin.
	
	center := v * (radius/len) + start.

	^ center