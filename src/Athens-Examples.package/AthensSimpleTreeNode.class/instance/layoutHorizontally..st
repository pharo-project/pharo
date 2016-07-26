accessing
layoutHorizontally: center
	| pos |
	"position ourselves in the middle X"
	originX := center - (self width * 0.5).
	
	"childs"
	pos := center - (self layoutWidth *0.5).
	
	children do: [ :each |
		each layoutHorizontally: pos + (each layoutWidth *0.5).
		pos := pos + each layoutWidth + self spacingBetweenChilds. 
	]