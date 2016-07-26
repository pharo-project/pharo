private
createResizableHeaderWith: aMorph between: leftColumn and: rightColumn
	"Create a wrapper morph with a resizable morph et the left (so we bind two columns). 
	 This morph will be completely transparent in all terms... it acts just as a container."
	^ Morph new 
		color: Color transparent;
		clipSubmorphs: true;
		layoutPolicy: FTRowLayout new;
		bounds: aMorph bounds;
		addMorphBack: (FTColumnResizerMorph 
			container: self 
			left: leftColumn 
			right: rightColumn);
		addMorphBack: aMorph;
		yourself