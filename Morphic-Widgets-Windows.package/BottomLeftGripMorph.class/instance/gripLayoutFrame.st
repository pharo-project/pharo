accessing
gripLayoutFrame
	^  (0 @ 1 corner: 0 @ 1) asLayoutFrame 
			topOffset: self defaultHeight negated;
			rightOffset: self defaultWidth