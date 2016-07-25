listeners
removeListener: anObject from: aListenerGroup 
	"Remove anObject from the given listener group. Return the new group."

	| listeners |
	aListenerGroup ifNil: [^nil].
	listeners := aListenerGroup.
	listeners := listeners copyWithout: anObject.
	listeners := listeners copyWithout: nil.	"obsolete entries"
	listeners isEmpty ifTrue: [listeners := nil].
	^listeners