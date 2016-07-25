as yet unclassified
radialBetween: origin
	extending: innerRadius
	and: outerOrigin
	extending: outerRadius
	withColorRamp: colorRamp
	| paint |

	paint := self
		primCreateRadialXo: origin x 
		yo: origin y 
		radiuso: innerRadius 
		x1: outerOrigin x
		y1: outerOrigin y 
		radius1: outerRadius.
	paint
		initializeRadialBetween: origin
			extending: innerRadius
			and: outerOrigin
			extending: outerRadius
			withColorRamp: colorRamp.
	^ paint.