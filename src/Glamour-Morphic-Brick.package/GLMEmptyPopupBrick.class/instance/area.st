brick-geometry
area
	| firstBounds secondBounds |

	firstBounds := self subbricks first outerBounds.
	secondBounds := self subbricks second outerBounds.

	^ GLMArea
		origin: firstBounds origin
		vectors: { 
			GLMVector east: firstBounds width.
			GLMVector south: firstBounds height.
			GLMVector horizontal: secondBounds corner x - firstBounds corner x.
			GLMVector south: secondBounds height.
			GLMVector west: secondBounds width.
			GLMVector north: secondBounds height.
			GLMVector horizontal: firstBounds origin x - secondBounds origin x.
			GLMVector north: firstBounds height
		}