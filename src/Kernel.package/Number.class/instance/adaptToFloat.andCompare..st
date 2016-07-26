converting
adaptToFloat: rcvr andCompare: selector 
	"If I am involved in comparison with a Float, convert rcvr to a
	Fraction. This way, no bit is lost and comparison is exact."
	
	rcvr isFinite
		ifFalse: [
			selector == #= ifTrue: [^false].
			selector == #~= ifTrue: [^true].
			rcvr isNaN ifTrue: [^ false].
			(selector = #< or: [selector = #'<='])
				ifTrue: [^ rcvr positive not].
			(selector = #> or: [selector = #'>='])
				ifTrue: [^ rcvr positive].
			^self error: 'unknow comparison selector'].

	^ rcvr asTrueFraction perform: selector with: self