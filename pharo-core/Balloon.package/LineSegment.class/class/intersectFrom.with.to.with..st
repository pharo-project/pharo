utilities
intersectFrom: startPt with: startDir to: endPt with: endDir
	"Compute the intersection of two lines, e.g., compute alpha and beta for
		startPt + (alpha * startDir) = endPt + (beta * endDir).
	Reformulating this yields
		(alpha * startDir) - (beta * endDir) = endPt - startPt.
	or
		(alpha * startDir) + (-beta * endDir) = endPt - startPt.
	or
		(alpha * startDir x) + (-beta * endDir x) = endPt x - startPt x.
		(alpha * startDir y) + (-beta * endDir y) = endPt y - startPt y.
	which is trivial to solve using Cramer's rule. Note that since
	we're really only interested in the intersection point we need only
	one of alpha or beta since the resulting intersection point can be
	computed based on either one."
	| det deltaPt alpha |
	det := (startDir x * endDir y) - (startDir y * endDir x).
	det = 0.0 ifTrue:[^nil]. "There's no solution for it"
	deltaPt := endPt - startPt.
	alpha := (deltaPt x * endDir y) - (deltaPt y * endDir x).
	alpha := alpha / det.
	"And compute intersection"
	^startPt + (alpha * startDir)