accessing
backgroundColor
	"Return the current fillStyle of the receiver."

	| basicColor size |
	basicColor := Smalltalk ui theme backgroundColor.
	self paragraph ifNil: [ ^Color transparent ].
	size := self textSize.
	size >= self warningLimit
		ifTrue: [ 
			|  overshoot transitionSpan transitionColor |
			transitionSpan := self warningLimit.
			transitionColor := Color yellow mixed: 0.6 with: basicColor.
			size >= self alertLimit
				ifFalse: [ overshoot := size - self warningLimit ]
				ifTrue: [ 
					transitionSpan := self alertLimit - self warningLimit.
					basicColor := transitionColor mixed: 0.5 with: basicColor.
					transitionColor := (Color orange mixed: 0.5 with: Color red) mixed: 0.5 with: basicColor.
					overshoot := size - self alertLimit ].
			^ CompositeFillStyle
				fillStyles:
					{((GradientFillStyle
						ramp:
							{(0 -> transitionColor).
							(1 -> basicColor)})
						origin: (self bounds width / 2) asFloat @ self bounds bottom;
						direction: 0 @ (12 + (2 * self bounds height * overshoot / transitionSpan)) negated asFloat;
						normal: 0 @ 1;
						radial: false;
						yourself).
					((GradientFillStyle
						ramp:
							{(0 -> Smalltalk ui theme backgroundColor).
							(0.3 -> (Smalltalk ui theme backgroundColor alpha: 0.01))})
						origin: self bounds left @ (self bounds height / 2) asFloat;
						direction: self bounds width @ 0;
						normal: 1 @ 0;
						radial: false;
						yourself)} ].
	^ self normalColor