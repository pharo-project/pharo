accessing colors
rowColorForEven: evenColor odd: oddColor
	rowColors at: 2 put: oddColor.
	rowColors at: 1 put: evenColor.
	