private
createClosedEllipsePathIn: aRectangle
	| radius width |
	aRectangle area > 0
		ifFalse: [ ^ self ].
	width := aRectangle width.
	radius := width / 2.
	^ self canvas
		createPath: [ :builder | 
			builder
				absolute;
				moveTo: width @ radius;
				cwArcTo: 0 @ radius angle: Float pi;
				cwArcTo: width @ radius angle: Float pi ]