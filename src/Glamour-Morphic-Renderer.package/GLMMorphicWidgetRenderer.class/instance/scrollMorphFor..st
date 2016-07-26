private
scrollMorphFor: aMorph
	^ GeneralScrollPane new
			changeScrollerTableLayout;
			scrollTarget: aMorph;
			layoutFrame: (LayoutFrame fractions: (0 @ 0 corner: 1 @ 1) offsets: (0 @ 25 corner: 0 @ 0));
			yourself
	