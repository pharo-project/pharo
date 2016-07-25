private
privateUnregisterExternalObject: anObject
	"Unregister the given object in the external objects array. Do nothing if it isn't registered.
	Calls to this method must be guarded against simultaneous removes from multiple threads."

	| objects |
	anObject ifNil: [^ self].
	objects := self unprotectedExternalObjects.
	1 to: objects size do: [:i |
		(objects at: i) == anObject ifTrue: 
		[objects at: i put: nil.
		^self]].
