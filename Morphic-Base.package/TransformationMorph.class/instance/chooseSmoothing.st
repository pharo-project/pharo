private
chooseSmoothing
	"Choose appropriate smoothing, after a change of scale or rotation."

	smoothing := (self scale < 1.0 or: [self angle ~= (self angle roundTo: Float pi / 2.0)]) 
		ifTrue: [ 2]
		ifFalse: [1]