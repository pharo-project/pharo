private
diffBaseFor: aDependency
	| wc |
	aDependency package hasWorkingCopy ifFalse: [^nil].
	wc := aDependency package workingCopy.
	wc ancestors ifEmpty: [^nil].
	^wc ancestors first name