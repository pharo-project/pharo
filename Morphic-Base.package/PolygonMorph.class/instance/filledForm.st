private
filledForm
	"Note: The filled form is actually 2 pixels bigger than bounds, and the point corresponding to this morphs' position is at 1@1 in the form.  This is due to the details of the fillig routines, at least one of which requires an extra 1-pixel margin around the outside.  Computation of the filled form is done only on demand."
	| bb origin |
	closed ifFalse: [^ filledForm := nil].
	filledForm ifNotNil: [^ filledForm].
	filledForm := Form extent: bounds extent+2.

	"Draw the border..."
	bb := (BitBlt toForm: filledForm) sourceForm: nil; fillColor: Color black;
			combinationRule: Form over; width: 1; height: 1.
	origin := bounds topLeft asIntegerPoint-1.
	self lineSegmentsDo: [:p1 :p2 | bb drawFrom: p1 asIntegerPoint-origin
										to: p2 asIntegerPoint-origin].

	"Fill it in..."
	filledForm convexShapeFill: Color black.

	(borderColor isColor and: [borderColor isTranslucentButNotTransparent]) ifTrue:
		["If border is stored as a form, then erase any overlap now."
		filledForm copy: self borderForm boundingBox from: self borderForm
			to: 1@1 rule: Form erase].

	^ filledForm