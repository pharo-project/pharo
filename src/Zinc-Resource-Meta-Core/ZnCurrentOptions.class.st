"
I am ZnCurrentOptions.
I am a DynamicVariable and a ProcessSpecificVariable.

I provide access to the current ZnOptions instance.

"
Class {
	#name : 'ZnCurrentOptions',
	#superclass : 'DynamicVariable',
	#category : 'Zinc-Resource-Meta-Core',
	#package : 'Zinc-Resource-Meta-Core'
}

{ #category : 'accessing' }
ZnCurrentOptions class >> at: key [
	^ self value
		ifNil: [ ZnOptions globalDefault at: key ]
		ifNotNil: [ :options | options at: key ]
]
