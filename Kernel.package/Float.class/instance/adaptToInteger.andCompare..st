converting
adaptToInteger: rcvr andCompare: selector 
	"If I am involved in comparison with an Integer, convert myself to a
	Fraction. This way, no bit is lost and comparison is exact."
	
	self isFinite
		ifFalse: [
			selector == #= ifTrue: [^false].
			selector == #~= ifTrue: [^true].
			self isNaN ifTrue: [^ false].
			(selector = #< or: [selector = #'<='])
				ifTrue: [^ self positive].
			(selector = #> or: [selector = #'>='])
				ifTrue: [^ self positive not].
			^self error: 'unknow comparison selector'].
		
	"Try to avoid asTrueFraction because it can cost"
	selector == #= ifTrue: [
		self fractionPart = 0.0 ifFalse: [^false]].
	selector == #~= ifTrue: [
		self fractionPart = 0.0 ifFalse: [^true]].
	
	^ rcvr perform: selector with: self asTrueFraction