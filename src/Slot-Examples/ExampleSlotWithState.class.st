"
I am a simple example for a Slot.

Instead of mapping to a field, I store the value myself. This means that all instances share the
slot, similar to a class variable.

I just overide the methods for reflective read and write (#read and #write:to:), I do not bother to emit bytecode myself but rely on the fallback that the compiler will generate code for reflective read and write (see the emit* method of my superclass).

Smalltalk classInstaller make: [ :builder |
	builder name: #A;
	slots: { ExampleSlotWithState named: #iv };
	category: #Playground ].


In class A we can implement accessors:

iv
	^iv

iv: anObject
	iv := anObject


They look like normal ivar accesses, but in the background, the compiler delegated to the Slot the code generation, calling emit*, which leads
to the bytecode e.g. for the read:

21 <20> pushConstant: iv => TestSlot
22 <70> self
23 <E1> send: read:
24 <7C> returnTop

you could override the emit* methods to generate faster code, but to get something running it is not needed.

To test, e.g. you can set the slot from one object and read it from another:

A new iv: 6
A new iv 

Then inspect the slot: 

(A slotNamed: #iv) inspect
"
Class {
	#name : #ExampleSlotWithState,
	#superclass : #Slot,
	#instVars : [
		'value'
	],
	#category : #'Slot-Examples-Base'
}

{ #category : #'meta-object-protocol' }
ExampleSlotWithState >> read: anObject [
	^ value
]

{ #category : #'meta-object-protocol' }
ExampleSlotWithState >> write: aValue to: anObject [
	value := aValue
]
