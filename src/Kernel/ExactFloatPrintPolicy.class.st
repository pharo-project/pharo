"
I am ExactFloatPrintPolicy.

Through FloatPrintPolicy and double dispatch I force Float>>#printOn:base: to dynamically use the slower but accurate way to print Floats using Float>>#absPrintExactlyOn:base:
"
Class {
	#name : #ExactFloatPrintPolicy,
	#superclass : #Object,
	#category : #Kernel-Numbers
}

{ #category : #printing }
ExactFloatPrintPolicy >> absPrint: float on: stream base: base [
	"Doube dispatch to the slower but accurate way to print"

	^ float absPrintExactlyOn: stream base: base
]
