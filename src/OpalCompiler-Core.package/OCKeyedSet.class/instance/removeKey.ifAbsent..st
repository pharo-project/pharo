removing
removeKey: key ifAbsent: aBlock
 
 	| index obj |
 	index := self findElementOrNil: key.
 	(obj := array at: index) ifNil: [ ^ aBlock value ].
 	array at: index put: nil.
 	tally := tally - 1.
 	self fixCollisionsFrom: index.
 	^ obj