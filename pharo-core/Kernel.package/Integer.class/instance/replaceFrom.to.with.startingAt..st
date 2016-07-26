system primitives
replaceFrom: start to: stop with: replacement startingAt: repStart
	| j |  "Catches failure if LgInt replace primitive fails"
	j := repStart.
	start to: stop do:
		[:i |
		self digitAt: i put: (replacement digitAt: j).
		j := j+1]