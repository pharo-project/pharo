displaying-display
follow: locationBlock while: durationBlock
   "Move an image around on the Display. Restore the background
   continuously without causing flashing. The argument, locationBlock,
   supplies each new location, and the argument, durationBlock, supplies
   true to continue, and then false to stop."
 
   | bitsBehind loc |
	loc := locationBlock value.
   bitsBehind := Form fromDisplay: (loc extent: self extent).
   ^ self follow: locationBlock while: durationBlock bitsBehind: bitsBehind startingLoc: loc