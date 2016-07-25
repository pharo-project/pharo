dropping/grabbing
rejectDropMorphEvent: evt
	"The receiver has been rejected, and must be put back somewhere.  There are three cases:
	(1)  It remembers its former owner and position, and goes right back there
	(2)  It remembers its former position only, in which case it was torn off from a parts bin, and the UI is that it floats back to its donor position and then vanishes."

	self formerOwner notNil 
		ifTrue: [^ self slideBackToFormerSituation: evt].

	self formerPosition  "Position but no owner -- can just make it vanish"
		ifNotNil: [^ self vanishAfterSlidingTo: self formerPosition event: evt].