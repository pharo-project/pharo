"
I am InexactFloatPrintPolicy.

Through FloatPrintPolicy and double dispatch I force Float>>#printOn:base: to dynamically use the faster but potentially less accurate way to print Floats using Float>>#absPrintOn:base:
"
Class {
	#name : #InexactFloatPrintPolicy,
	#superclass : #Object,
	#category : 'Kernel-Numbers'
}

{ #category : #printing }
InexactFloatPrintPolicy >> absPrint: float on: stream base: base [
	"Doube dispatch to the faster but potentially less accurate way to print"

	^ float absPrintInexactlyOn: stream base: base 
]
