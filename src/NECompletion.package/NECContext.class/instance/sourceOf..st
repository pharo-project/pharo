private
sourceOf: aSHRange 
	^aSHRange isString
		ifTrue: [aSHRange]
		ifFalse: [self sourceOf: aSHRange stopAt: aSHRange end]