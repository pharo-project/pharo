vector functions
parameterAtExtreme: tangentDirection
	"Compute the parameter value at which the tangent reaches tangentDirection.
	We need to find the parameter value t at which the following holds

		((t * dir + in) crossProduct: tangentDirection) = 0.

	Since this is pretty ugly we use the normal direction rather than the tangent and compute the equivalent relation using the dot product as

		((t * dir + in) dotProduct: nrm) = 0.

	Reformulation yields

		((t * dir x + in x) * nrm x) + ((t * dir y + in y) * nrm y) = 0.
		(t * dir x * nrm x) + (in x * nrm x) + (t * dir y * nrm y) + (in y * nrm y) = 0.
		(t * dir x * nrm x) + (t * dir y * nrm y) = 0 - ((in x * nrm x) + (in y * nrm y)).

				(in x * nrm x) + (in y * nrm y)
		t = 0 -	---------------------------------------
			 	(dir x * nrm x) + (dir y * nrm y)
	And that's that. Note that we can get rid of the negation by computing 'dir' the other way around (e.g., in the above it would read '-dir') which is trivial to do. Note also that the above does not generalize easily beyond 2D since its not clear how to express the 'normal direction' of a tangent plane.
	"
	| inX inY dirX dirY nrmX nrmY |
	"Compute in"
	inX := via x - start x.
	inY := via y - start y.
	"Compute -dir"
	dirX := inX - (end x - via x).
	dirY := inY - (end y - via y).
	"Compute nrm"
	nrmX := tangentDirection y.
	nrmY := 0 - tangentDirection x.
	"Compute result"
	^((inX * nrmX) + (inY * nrmY)) / 
		((dirX * nrmX) + (dirY * nrmY))