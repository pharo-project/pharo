events
outerPortEvent: aPortEvent
	self announce: (GLMContextChanged new
				presentation: self; 
				property: aPortEvent portName; 
				value: aPortEvent value; 
				oldValue: aPortEvent oldValue; 
				yourself).
	self portChangeActions do: [ :each | 
		each portName = aPortEvent portName ifTrue: [ 
			each actOn: self ] ]