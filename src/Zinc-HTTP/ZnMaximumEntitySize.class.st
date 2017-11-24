"
I am ZnMaximumEntitySize.
I am a DynamicVariable and a ProcessSpecificVariable.

I am used to control the maximum size of entities being read from streams.

"
Class {
	#name : #ZnMaximumEntitySize,
	#superclass : #DynamicVariable,
	#category : #'Zinc-HTTP-Variables'
}

{ #category : #accessing }
ZnMaximumEntitySize >> default [
	"By default, allow for entities of up to 16 Mb to be read from streams"
	
	^ 16 * 1024 * 1024
]
