tests - accessing
testWithStartStopAndValueDo
	"self debug: #testWithStartStopAndValueDo"
	| array elements startStops |
	array := #($a $b $b $c $c $c $d $d) as: RunArray.
	elements := OrderedCollection new.
	startStops := OrderedCollection new.
	array withStartStopAndValueDo: [:start :stop :value | elements add: value. startStops add: start->stop].
	
	self assert: elements asArray = #($a $b  $c  $d).
	self assert: startStops asArray = {1->1 . 2->3 . 4->6 . 7->8}