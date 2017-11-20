"
I am a strategy that help tests for writing (and then reading) on streams.
"
Class {
	#name : #FLStreamStrategy,
	#superclass : #Object,
	#category : #FuelTests-StreamStrategies
}

{ #category : #serializing }
FLStreamStrategy >> fuelAccept: aGeneralMapper [
	"Visit myself as a substitution. See #fuelSubstitution."

	^ aGeneralMapper visitSubstitution: self by: nil
]

{ #category : #reading }
FLStreamStrategy >> readStreamDo: aValuable [
	"Evaluates the argument with a read stream. Answer the result."
	
	self subclassResponsibility 
]

{ #category : #'test support' }
FLStreamStrategy >> tearDown [
]

{ #category : #writing }
FLStreamStrategy >> writeStreamDo: aValuable [
	"Evaluates the argument with a write stream. Answer the result."
	
	self subclassResponsibility 
]
