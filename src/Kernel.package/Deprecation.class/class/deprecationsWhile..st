logging
deprecationsWhile: aBlock
	"returns a log of all deprecated methods seen while executing aBlock"
	| oldLog result |
	oldLog := Log.
	Log := Set new.
	aBlock value.
	result := Log.
	oldLog ifNotNil: [oldLog addAll: result].
	Log := oldLog.
	^result