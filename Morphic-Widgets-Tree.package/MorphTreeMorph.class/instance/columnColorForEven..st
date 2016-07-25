accessing colors
columnColorForEven: evenColor 
	columnColors at: 2 put: nil.
	columnColors at: 1 put: evenColor.
	