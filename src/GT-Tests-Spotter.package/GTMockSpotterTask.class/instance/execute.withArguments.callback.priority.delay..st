public
execute: aBlock withArguments: aCollection callback: callbackBlock priority: aPriority delay: anIntegerMilliseconds
	" we cannot handle asynchronous processes well in tests, so just execute is synchronously "
	aBlock valueWithPossibleArgs: aCollection.
	aCollection value.