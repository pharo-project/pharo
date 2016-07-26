displaying-display
follow: locationBlock while: durationBlock bitsBehind: initialBitsBehind startingLoc: loc
   "Move an image around on the Display. Restore the background continuously without causing flashing. The argument, locationBlock, supplies each new location, and the argument, durationBlock, supplies true to continue or false to stop. This variant takes the bitsBehind as an input argument, and returns the final saved saved bits as method value."

   | location rect1 save1 save1Blt buffer bufferBlt newLoc rect2 bothRects |
   location := loc.
   rect1 := location extent: self extent.
   save1 := initialBitsBehind.
   save1Blt := BitBlt toForm: save1.
   buffer := Form extent: self extent*2 depth: Display depth.  "Holds overlapping region"
   bufferBlt := BitBlt toForm: buffer.

   Display deferUpdates: true.
   self displayOn: Display at: location rule: Form paint.
   Display deferUpdates: false; forceToScreen: (location extent: self extent).

   [durationBlock value] whileTrue: [
		newLoc := locationBlock value.
		newLoc ~= location ifTrue: [
			rect2 := newLoc extent: self extent.
			bothRects := rect1 merge: rect2.
			(rect1 intersects: rect2)
				ifTrue: [  "when overlap, buffer background for both rectangles"
					bufferBlt copyFrom: bothRects in: Display to: 0@0.
					bufferBlt copyFrom: save1 boundingBox in: save1 to: rect1 origin - bothRects origin.
					"now buffer is clean background; get new bits for save1"
					save1Blt copy: (0@0 extent: self extent) from: rect2 origin - bothRects origin in: buffer.
					self displayOnPort: bufferBlt at: rect2 origin - bothRects origin rule: Form paint.
					Display deferUpdates: true.
					Display copy: bothRects from: 0@0 in: buffer rule: Form over.
					Display deferUpdates: false; forceToScreen: bothRects]
				ifFalse: [  "when no overlap, do the simple thing (both rects might be too big)"
					Display deferUpdates: true.
					Display copy: (location extent: save1 extent) from: 0@0 in: save1 rule: Form over.
					save1Blt copyFrom: rect2 in: Display to: 0@0.
					self displayOn: Display at: newLoc rule: Form paint.
					Display deferUpdates: false; 
						forceToScreen: (location extent: save1 extent); 
						forceToScreen: (newLoc extent: self extent)].
			location := newLoc.
			rect1 := rect2]].

	^ save1 displayOn: Display at: location
