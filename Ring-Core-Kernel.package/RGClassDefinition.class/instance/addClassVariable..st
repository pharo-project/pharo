class variables
addClassVariable: aRCClassVariable

	self addVariable: (aRCClassVariable parent: self) 
		in: classVariables