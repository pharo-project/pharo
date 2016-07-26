logging
deprecationsWhile: aBlock
	| oldLog result |
	oldLog := Log.
	Log := Set new.
	aBlock value.
	result := Log.
	oldLog ifNotNil: [oldLog addAll: result].
	Log := oldLog.
	^result