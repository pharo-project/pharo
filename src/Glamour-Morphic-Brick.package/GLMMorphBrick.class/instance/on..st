instance creation
on: aMorph
	
	aMorph layoutProperties ifNil: [ 
		aMorph layoutProperties: GLMBrickLayoutProperties new.
		aMorph layoutProperties hSpaceFill; vSpaceFill ].
	(aMorph layoutProperties isKindOf: GLMBrickLayoutProperties) ifTrue: [
		self layoutProperties: aMorph layoutProperties ].

	morph := aMorph.
	morph layoutProperties: (GLMBrickLayoutProperties new).
	morph layoutProperties hSpaceFill; vSpaceFill.
	
	self addBrickBack: morph.
	super brickBounds: (0@0 extent: (morph width@morph height)).