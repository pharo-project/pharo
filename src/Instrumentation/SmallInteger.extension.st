Extension { #name : 'SmallInteger' }

{ #category : '*Instrumentation' }
SmallInteger >> insInequality: anObj [

    <primitive: 8>
    <noInstrumentation>
    ^ super insInequality: anObj
]

{ #category : '*Instrumentation' }
SmallInteger >> insIsEqual: aNumber [
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is equal to the argument. Otherwise answer false. Fail if the
	argument is not a SmallInteger. Essential. No Lookup. See Object
	documentation whatIsAPrimitive. "

	<primitive: 7>
	<noInstrumentation>
	^super insIsEqual: aNumber
]

{ #category : '*Instrumentation' }
SmallInteger >> insMinus: aNumber [
    "I am a re-implementation of the method SmallInteger>>#- and I have the same code except
	for the instrumentation pragmas. I re-implemented the method just to be able to use it
	without poluting the instrumentation. Let's say that one wants to instrument the method
	SmallInteger>>#-. If we not override the  SmallInteger>>#- method we will enter to an
	endless loop."

    <metaLinkOptions: #( #+ optionDisabledLink )>
    <noInstrumentation>
    <primitive: 2>
    ^ super - aNumber
]

{ #category : '*Instrumentation' }
SmallInteger >> insPlus: aNumber [
	"I am a re-implementation of the method SmallInteger>>#+ and I have the same code except
	for the instrumentation pragmas. I re-implemented the method just to be able to use it
	without poluting the instrumentation. Let's say that one wants to instrument the method
	SmallInteger>>#+. If we not override the  SmallInteger>>#+ method we will enter to an
	endless loop."

	<metaLinkOptions: #( #+ optionDisabledLink )>
	<noInstrumentation>
	<primitive: 1>
	^ super + aNumber
]
