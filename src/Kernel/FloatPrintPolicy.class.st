"
I am FloatPrintPolicy.
I am a DynamicVariable.

I control whether Float instances are printed exactly or inexactly. The inexact printing is much faster, but can be less accurate. 

The default policy is ExactFloatPrintPolicy.

FloatPrintPolicy 
	value: InexactFloatPrintPolicy new 
	during: [ Float pi printString ]
"
Class {
	#name : #FloatPrintPolicy,
	#superclass : #DynamicVariable,
	#instVars : [
		'default'
	],
	#category : 'Kernel-Numbers'
}

{ #category : #printing }
FloatPrintPolicy class >> absPrint: float on: stream base: base [
	"I delegate Float printing to the current dynamic value of myself"

	self value absPrint: float on: stream base: base
]

{ #category : #accessing }
FloatPrintPolicy >> default [
	^ default
]

{ #category : #initialization }
FloatPrintPolicy >> initialize [
	default := ExactFloatPrintPolicy new
]
