tests
testIncludesAny
	self deny: (aTimespan includesAnyOf: (Bag with: dec31)).
	self assert: (aTimespan includesAnyOf: (Bag with: jan01 with: jan08))
	"Error is due to bug in Timespan 
includesAnyOf: aCollection "
	"Answer whether any element of aCollection is included in the receiver"
	"aCollection do: [ :elem | (self includes: elem) ifTrue: [^ true]].
Shouldn't this return false if none are included?
"
