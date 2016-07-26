accessing
priority

	^ priority ifNil: [ priority := Processor userSchedulingPriority ]