initialization
addMainMorphsWith: topOffset 
	| halfWidth leftFraction leftOffset rightFraction rightOffset |
	
	halfWidth := self joinMorph width / 2.
	leftFraction := showOnlyDestination
		ifTrue: [ 0 ]
		ifFalse: [ 0.5 ].
		
	leftOffset := showOnlyDestination
		ifTrue: [ 0 ]
		ifFalse: [ halfWidth ].
		
	rightFraction := showOnlySource
		ifTrue: [ 1 ]
		ifFalse: [ 0.5 ].
		
	rightOffset := showOnlySource
		ifTrue: [ 0 ]
		ifFalse: [ halfWidth negated ].
		 
	showOnlyDestination ifFalse: [
		self
			addMorph: self srcMorph
			fullFrame: (LayoutFrame identity
				rightFraction: rightFraction;
				topOffset: topOffset;
				rightOffset: rightOffset).
	showOnlySource ifFalse: [
		self		
			addMorph: self joinMorph
			fullFrame: (LayoutFrame identity
				leftFraction: 0.5;
				rightFraction: 0.5;
				leftOffset: halfWidth negated;
				rightOffset: halfWidth;
				topOffset: topOffset).
		] ].
	showOnlySource ifFalse: [
		self
			addMorph: self dstMorph
			fullFrame: (LayoutFrame identity
				leftFraction: leftFraction;
				topOffset: topOffset;
				leftOffset: leftOffset;
				rightOffset: (self scrollbarMorph width + self mapMorph width) negated) ].
				
	self				
		addMorph: self scrollbarMorph
		fullFrame: (LayoutFrame identity
			leftFraction: 1;
			leftOffset: self scrollbarMorph width negated - self mapMorph width;
			rightOffset: self mapMorph width negated;
			topOffset: topOffset).
	self					
		addMorph: self mapMorph
		fullFrame: (LayoutFrame identity
			leftFraction: 1;
			leftOffset: self mapMorph width negated;
			topOffset: topOffset).