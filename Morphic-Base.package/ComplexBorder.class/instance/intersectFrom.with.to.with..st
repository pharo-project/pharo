private
intersectFrom: startPt with: startDir to: endPt with: endDir
	"Compute the intersection of two lines. Return nil if either
		* the intersection does not exist, or
		* the intersection is 'before' startPt, or
		* the intersection is 'after' endPt
	"
	| det deltaPt alpha beta |
	det := (startDir x * endDir y) - (startDir y * endDir x).
	det = 0.0 ifTrue:[^nil]. "There's no solution for it"
	deltaPt := endPt - startPt.
	alpha := (deltaPt x * endDir y) - (deltaPt y * endDir x).
	beta := (deltaPt x * startDir y) - (deltaPt y * startDir x).
	alpha := alpha / det.
	beta := beta / det.
	alpha < 0 ifTrue:[^nil].
	beta > 1.0 ifTrue:[^nil].
	"And compute intersection"
	^(startPt x + (alpha * startDir x)) @ (startPt y + (alpha * startDir y))