smoothing
lineSegmentsDo: endPointsBlock 
	"Emit a sequence of segment endpoints into endPointsBlock."
	"Unlike the method this one replaces we expect the curve 
	coefficents not the dirivatives"
	"Also unlike the replaced method the smooth closed curve
	does 
	not need an extra vertex. 
	We take care of the extra endpoint here. Just like for 
	segmented curves."
	| cs x y beginPoint |
	vertices size < 1
		ifTrue: [^ self].
	"test too few vertices first"
	self isCurvy
		ifFalse: [beginPoint := nil.
			"smoothCurve 
			ifTrue: [cs := self coefficients]."
			"some things still depend on smoothCurves having 
			curveState"
			vertices
				do: [:vert | 
					beginPoint
						ifNotNil: [endPointsBlock value: beginPoint value: vert].
					beginPoint := vert].
			endPointsBlock value: beginPoint value: vertices first.
			^ self].
	"For curves we include all the interpolated sub segments."
	"self assert: [(vertices size > 2 )].	"
	cs := self coefficients.
	beginPoint := (x := cs first first) @ (y := cs fifth first).
	1 to: cs first size
		do: [:i | | t n x3 y3 x1 endPoint x2 y1 y2 | 
			"taylor series coefficients"
			x1 := cs second at: i.
			y1 := cs sixth at: i.
			x2 := cs third at: i.
			y2 := cs seventh at: i.
			x3 := cs fourth at: i.
			y3 := cs eighth at: i.
			n := cs ninth at: i.
			"guess n 
			n := 5 max: (x2 abs + y2 abs * 2.0 + (cs third atWrap:
			i 
			+ 1) abs + (cs seventh atWrap: i + 1) abs / 100.0) 
			rounded."
			1
				to: n - 1
				do: [:j | 
					t := j asFloat / n asFloat.
					endPoint := x3 * t + x2 * t + x1 * t + x @ (y3 * t + y2 * t + y1 * t + y).
					endPointsBlock value: beginPoint value: endPoint.
					beginPoint := endPoint].
			endPoint := (x := cs first atWrap: i + 1) @ (y := cs fifth atWrap: i + 1).
			endPointsBlock value: beginPoint value: endPoint.
			beginPoint := endPoint]