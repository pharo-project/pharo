private
failedRules
	^self isEmpty
		ifTrue: [#()]
		ifFalse: [Array with: self]