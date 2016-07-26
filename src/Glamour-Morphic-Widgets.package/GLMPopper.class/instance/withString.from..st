private
withString: string from: aMorph
	| top width left stringMorph height desiredOrigin |
	desiredOrigin := self computeOriginFor: aMorph.
	stringMorph := RubEditingArea new 
		font: self theme textFont;
		updateTextWith: string.
	width := stringMorph width + 12 min: World width / 3.
	left := desiredOrigin x + width > World width
		ifTrue: [ World width - width ]
		ifFalse: [ desiredOrigin x ].
	height := (stringMorph
		beWrapped;
		width: width;
		yourself) height + 2 min: World height / 3.
	top := desiredOrigin y + height > World height
		ifTrue: [ World height - height ]
		ifFalse: [ desiredOrigin y ].
	self updateWithString: string from: aMorph.
	self
		top: top;
		left: left;
		width: width;
		height: height