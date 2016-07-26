instance creation
sample
	"GradientFill sample"
	^(self ramp: { 0.0 -> Color red. 0.5 -> Color green. 1.0 -> Color blue})
		origin: 300 @ 300;
		direction: 400@0;
		normal: 0@400;
		radial: true;
	yourself