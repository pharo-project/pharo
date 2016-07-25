private
addDirectionHandles

	| centerHandle d w directionShaft patch patchColor crossHairColor |
	self showingDirectionHandles ifFalse: [^ self].

	directionArrowAnchor := (target point: target referencePosition in: self world) rounded.
	patch := target imageFormForRectangle: (Rectangle center: directionArrowAnchor extent: 3@3).
	patchColor := patch colorAt: 1@1.

	(directionShaft := LineMorph newSticky makeForwardArrow)
		borderWidth: 2; borderColor: (Color green orColorUnlike: patchColor).
	self positionDirectionShaft: directionShaft.
	self addMorphFront: directionShaft.
	directionShaft setCenteredBalloonText: 'Set forward direction' translated;
		on: #mouseDown send: #doDirection:with: to: self;
		on: #mouseMove send: #trackDirectionArrow:with: to: self;
		on: #mouseUp send: #setDirection:with: to: self.

	d := 15.  "diameter"  w := 3.  "borderWidth"
	crossHairColor := Color red orColorUnlike: patchColor.
	(centerHandle := EllipseMorph newBounds: (0@0 extent: d@d) color: Color transparent)
			borderWidth: w; borderColor: (Color blue orColorUnlike: patchColor);
			addMorph: (LineMorph from: (d//2)@w to: (d//2)@(d-w-1) color: crossHairColor width: 1) lock;
			addMorph: (LineMorph from: w@(d//2) to: (d-w-1)@(d//2) color: crossHairColor width: 1) lock;
			align: centerHandle bounds center with: directionArrowAnchor.
	centerHandle wantsYellowButtonMenu: false.
	self addMorph: centerHandle.
	centerHandle setCenteredBalloonText: 'Rotation center (hold down the shift key and drag from here to change it)' translated;
			on: #mouseDown send: #prepareToTrackCenterOfRotation:with: to: self;
			on: #mouseMove send: #trackCenterOfRotation:with: to: self;
			on: #mouseUp send: #setCenterOfRotation:with: to: self
