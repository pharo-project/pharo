accessing colors
columnColorForEven: evenColor odd: oddColor
	columnColors at: 2 put: oddColor.
	columnColors at: 1 put: evenColor.
	