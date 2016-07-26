*Polymorph-Widgets
fastAnimateRectangles: rects
	"Animate the given rectangles using the fast method."
	
	|delay col merged|
	rects ifEmpty: [^self].
	delay := Delay forMilliseconds: (self theme settings animationSettings delay).
	col := Color gray alpha: 0.5.
	merged := rects first.
	rects withIndexDo: [:r :i |
		Display
			border: r rounded
			width: 1
			rule: Form blend
			fillColor: col.
		merged := merged merge: r.
		delay wait].
	merged := merged expandBy: 1.
	World invalidRect: merged from: self