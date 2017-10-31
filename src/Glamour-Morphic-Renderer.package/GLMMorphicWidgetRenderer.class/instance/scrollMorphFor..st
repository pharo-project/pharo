private
scrollMorphFor: aMorph
	^ GeneralScrollPane new
			changeScrollerTableLayout;
			scrollTarget: aMorph;
			layoutFrame: (LayoutFrame identity topOffset: 25);
			yourself
	