A TransformMorph introduces a 2-D transformation between its (global) coordinates and the (local) coordinates of its submorphs, while also clipping all display to its bounds.  Specifically, with no offset, angle or scaling, a submorph with coordinates (0@0) will appear exactly at the topLeft of the windowMorph (its position).  Rotation and scaling are relative to the local origin, (0@0).

instance var	type				description
 transform		MorphicTransform	The coordinate transform between my coordinates and the
									local coordinates of my submorphs.
 smoothing		anInteger in 1..3	Perform smoothing of my contents during drawing
										1 No smoothing (#smoothingOff)
										2 Smoothing w/ edge adjacent pixels (#smoothingOn)
										3 Smoothing w/ edge and corner adj pixels
			
 localBounds	Rectangle or nil		caches the value of #localSubmorphBounds for performance

TransformMorphs operate with two different display strategies, depending on whether the transformation is a pure translation or not.  If so, then they simply use a clipping canvas and display their submorphs with the appropriate offset.  If the transformation includes scaling or rotation, then a caching canvas is used, whose active area covers the fullBounds of the submorphs intersected with the source quadrilateral corresponding to the window bounds.