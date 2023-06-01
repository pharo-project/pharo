Extension { #name : #SmallInteger }

{ #category : #'*Instrumentation' }
SmallInteger >> insMinus: aNumber [
	"Primitive. Subtract the argument from the receiver and answer with the
	result if it is a SmallInteger. Fail if the argument or the result is not a
	SmallInteger. Essential. No Lookup. See Object documentation
	whatIsAPrimitive."
	<metaLinkOptions: #( + optionDisabledLink)>
	<primitive: 2>
	^super - aNumber
]

{ #category : #'*Instrumentation' }
SmallInteger >> insPlus: aNumber [
	"Primitive. Add the receiver to the argument and answer with the result
	if it is a SmallInteger. Fail if the argument or the result is not a
	SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive."
	<metaLinkOptions: #( + optionDisabledLink)>
	<primitive: 1>
	^ super + aNumber
]
