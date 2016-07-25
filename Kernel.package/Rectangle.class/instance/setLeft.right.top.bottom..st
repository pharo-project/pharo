private
setLeft: left right: right top: top bottom: bottom 

	origin := (left min: right) @ (top min: bottom).
	corner := (left max: right) @ (top max: bottom).
