morph-drawing
drawOnAthensCanvas: anAthensCanvas

	anAthensCanvas pathTransform restoreAfter: [
					anAthensCanvas pathTransform translateBy: (self globalBounds origin - self brickBounds origin).
					self onDrawAthensCanvas: anAthensCanvas ]