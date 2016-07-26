accessing
stringMorph
	^ stringMorph
		ifNil: [ 
			stringMorph := LabelMorph new
				enabled: false;
				vResizing: #shrinkWrap;
				hResizing: #spaceFill;
				yourself ]