views
stackListKey: aKey from: aView 
	aKey = $c ifTrue: [ self inspectContext].
	aKey = $C ifTrue: [ self exploreContext].
	aKey = $i ifTrue: [ self inspectReceiver].
	aKey = $I ifTrue: [ self exploreReceiver].
	aKey = $b ifTrue: [ self browseContext].

