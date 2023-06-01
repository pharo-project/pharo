Class {
	#name : #InstrumentationEnsurer,
	#superclass : #Object,
	#category : #Instrumentation
}

{ #category : #'instance creation' }
InstrumentationEnsurer class >> basicNew [
	"Redefining basicNew on this enables us to exclude it from instrumentation and be able to 	instrument Behavior>>basicNew without creating infinite loops"

	"Primitive. Answer an instance of the receiver (which is a class) with no
	 indexable variables. Fail if the class is indexable. Essential. See Object
	 documentation whatIsAPrimitive.

	 If the primitive fails because space is low then the scavenger will run
	 before the method is activated.  Check that space was low and retry
	 via handleFailingBasicNew if so."

	<primitive: 70 error: ec>
	<noInstrumentation>
	ec == #'insufficient object memory' ifTrue: [
		^ self handleFailingBasicNew ].
	self isVariable ifTrue: [ ^ self basicNew: 0 ].
	self primitiveFailed
]

{ #category : #accessing }
InstrumentationEnsurer >> valueUnwindInContext: aContext [

	<noInstrumentation>
	self value
]
