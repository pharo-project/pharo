accessing
scaledImage
	"Answer the image scaled as required."
	
	|i|
	i := self form.
	i boundingBox area = 0 ifTrue: [^i].
	(self layout == #scaled and: [self extent ~= i extent]) ifTrue: [
		^i magnify: i boundingBox by: (self extent / i extent) smoothing: 2].
	(self layout == #scaledAspect and: [self extent ~= i extent]) ifTrue: [
		^self width / i width > (self height / i height)
			ifTrue: [i magnify: i boundingBox by: (self height / i height) smoothing: 2]
			ifFalse: [i magnify: i boundingBox by: (self width / i width) smoothing: 2]].
	self scale ~= 1 ifTrue: [
		^i magnify: i boundingBox by: self scale smoothing: 2].
	^i