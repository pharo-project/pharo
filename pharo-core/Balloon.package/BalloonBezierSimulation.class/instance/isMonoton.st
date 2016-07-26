computing
isMonoton
	"Return true if the receiver is monoton along the y-axis,
	e.g., check if the tangents have the same sign"
	^(via y - start y) * (end y - via y) >= 0