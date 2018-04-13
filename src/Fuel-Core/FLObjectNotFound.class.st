"
I represent an error which may happen during serialization, when trying to encode on the stream a reference to an object that should be encoded before, but it is not.

This usually happens when the graph changes during serialization. 

Another possible cause is a bug in the analysis step of serialization.
"
Class {
	#name : #FLObjectNotFound,
	#superclass : #FLSerializationError,
	#category : #'Fuel-Core-Errors'
}

{ #category : #signaling }
FLObjectNotFound class >> signalWith: anObject [

 	self signal: (
		String streamContents: [ :stream |
			stream << 'Unexpected reference to '.
			stream print: anObject.
			stream << ' in the graph. This usually happens when the graph changes during serialization.'.
			]
	)
]
