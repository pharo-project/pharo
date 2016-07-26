line drawing
drawLoopX: xDelta Y: yDelta 
	"Primitive. Implements the Bresenham plotting algorithm (IBM Systems
	Journal, Vol. 4 No. 1, 1965). It chooses a principal direction, and
	maintains a potential, P. When P's sign changes, it is time to move in
	the minor direction as well. This particular version does not write the
	first and last points, so that these can be called for as needed in client code.
	Optional. See Object documentation whatIsAPrimitive."
	| dx dy px py P |
	<primitive: 'primitiveDrawLoop' module: 'BitBltPlugin'>
	dx := xDelta sign.
	dy := yDelta sign.
	px := yDelta abs.
	py := xDelta abs.
	"self copyBits."
	py > px 
		ifTrue: 
			[ "more horizontal"
			P := py // 2.
			1 
				to: py
				do: 
					[ :i | 
					destX := destX + dx.
					(P := P - px) < 0 ifTrue: 
						[ destY := destY + dy.
						P := P + py ].
					i < py ifTrue: [ self copyBits ] ] ]
		ifFalse: 
			[ "more vertical"
			P := px // 2.
			1 
				to: px
				do: 
					[ :i | 
					destY := destY + dy.
					(P := P - py) < 0 ifTrue: 
						[ destX := destX + dx.
						P := P + px ].
					i < px ifTrue: [ self copyBits ] ] ]