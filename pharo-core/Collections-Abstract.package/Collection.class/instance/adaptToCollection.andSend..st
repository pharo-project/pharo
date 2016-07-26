adapting
adaptToCollection: rcvr andSend: selector
	"If I am involved in arithmetic with another Collection, return a Collection of
	the results of each element combined with the scalar in that expression."

	(rcvr isSequenceable and: [ self isSequenceable ]) ifFalse:
		[self error: 'Only sequenceable collections may be combined arithmetically'].
	^ rcvr with: self collect:
		[:rcvrElement :myElement | rcvrElement perform: selector with: myElement]