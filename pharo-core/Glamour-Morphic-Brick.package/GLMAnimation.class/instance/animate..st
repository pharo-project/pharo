scripting
animate: anObject

	self asyncTask terminate.
	animatedObject := anObject.
	self reset.
	
	stamp := Time millisecondClockValue.
	self asyncTask
		doInBackground: [ [ self shouldStop ] whileFalse: [
				self needsStep ifTrue: [ self animationStep ] ] ];
		onCompleted: [ 
			isCompleted := true.
			[ callback brickValue: self animatedObject ]
				ensure: [ ensureLogic brickValue: self animatedObject ] ];
		priority: Processor userSchedulingPriority;
		execute