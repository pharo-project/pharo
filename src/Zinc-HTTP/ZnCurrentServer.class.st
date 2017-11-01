"
I am ZnCurrentServer.
I am a DynamicVariable and a ProcessSpecificVariable.

I hold a reference to the current ZnServer instance during HTTP processing, during the dynamic scope of #handleRequest:
"
Class {
	#name : #ZnCurrentServer,
	#superclass : #DynamicVariable,
	#category : 'Zinc-HTTP-Variables'
}
