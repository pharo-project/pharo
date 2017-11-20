"
This is the default strategy that does nothing (an implementation of the Null pattern)
"
Class {
	#name : #GLMNoStrategy,
	#superclass : #GLMTransmissionStrategy,
	#category : #Glamour-Core
}

{ #category : #transmitting }
GLMNoStrategy >> transmitIn: aContext [
	"do nothing else then was already done by the transmission"
]
