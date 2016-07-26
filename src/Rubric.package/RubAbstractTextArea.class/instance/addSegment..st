accessing segments
addSegment: aRubTextSegmentMorph
	| seg |
	seg := aRubTextSegmentMorph inTextArea: self.
	self segments add: seg.
	self addMorphBack: seg.
	^ seg
