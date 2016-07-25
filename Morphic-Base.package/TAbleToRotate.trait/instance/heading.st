t-rotating
heading
	"Return the receiver's heading"
	^ self owner 
		ifNil: [self forwardDirection]
		ifNotNil: [self forwardDirection + self owner degreesOfFlex]