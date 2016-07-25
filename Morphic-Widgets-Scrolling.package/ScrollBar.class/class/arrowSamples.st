images - samples
arrowSamples
	"create a set of arrow with different sizes, colors and directions"
	" 
	ScrollBar arrowSamples.  
	"
	| column |
	column := AlignmentMorph newColumn vResizing: #shrinkWrap;
				 hResizing: #shrinkWrap;
				 layoutInset: 1;
				 borderColor: Color black;
				 borderWidth: 0;
				 wrapCentering: #center;
				 cellPositioning: #center;
				 color: Color white;
				 yourself.
	
	self sampleSizes
		do: [:size | 
			| row | 
			row := AlignmentMorph newRow color: Color transparent;
						 vResizing: #shrinkWrap;
						 cellInset: 2 @ 0 yourself.
			
			self sampleColors
				do: [:color | 
					#(#top #right #bottom #left )
						do: [:direction | 
							row addMorphBack: (ScrollBar
									arrowOfDirection: direction
									size: size
									color: color) asMorph]].
			
			column addMorphBack: row].
	
	column openInHand