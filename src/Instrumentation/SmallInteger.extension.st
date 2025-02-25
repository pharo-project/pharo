Extension { #name : 'SmallInteger' }

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
