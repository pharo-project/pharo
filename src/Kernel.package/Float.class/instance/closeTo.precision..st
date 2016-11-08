comparing
closeTo: num precision: aPrecision
 	"Tell whether the receiver and arguments are close from each other given a precision"
	
	num isNumber ifFalse: [ ^ [self = num] ifError: [false]].
	self = 0.0 ifTrue: [^ num abs < aPrecision].
	num = 0 ifTrue: [^self abs < aPrecision].
	^self = num asFloat
		or: [(self - num) abs / (self abs max: num abs) < aPrecision]