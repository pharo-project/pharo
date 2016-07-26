signalling
signalFor: aClass
	self signal: aClass name asString, ': Trying to build a circular hierarchy'