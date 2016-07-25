mirror primitives
object: anObject basicAt: index put: value 
	"Store the last argument 
	 value in the indexable element of the argument anObject indicated by index without sending
	 anObject a message. Fail if the argument index is not an Integer or is out of bounds, or if
	 anObject is not indexable, or if value is an inappropriate value for anObject's indexable slots.
	 This mimics the action of the VM when it indexes an object.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 61>
	index isInteger
		ifTrue: [(index >= 1 and: [index <= (self objectSize: anObject)])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	index isNumber
		ifTrue: [^self object: anObject basicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]