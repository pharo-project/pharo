rendering
athensFillRectangle: aRect on: anAthensCanvas
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "
	|rotation normal|
	rotation := anAthensCanvas surface paintTransform transform: direction.
	normal := anAthensCanvas surface paintTransform transform:self normal.
	

	anAthensCanvas surface engine reset
		prepareForRendering;
		registerFormFill: form origin: origin direction: rotation normal: normal tileFlag: tileFlag;
		fillRectangle: aRect
