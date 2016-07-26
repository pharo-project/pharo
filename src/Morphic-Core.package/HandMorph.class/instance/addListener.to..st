listeners
addListener: anObject to: aListenerGroup
	"Add anObject to the given listener group. Return the new group."
	| listeners |
	listeners := aListenerGroup.
	(listeners notNil and:[listeners includes: anObject]) ifFalse:[
		listeners
			ifNil:[listeners := WeakArray with: anObject]
			ifNotNil:[listeners := listeners copyWith: anObject]].
	listeners := listeners copyWithout: nil. "obsolete entries"
	^listeners