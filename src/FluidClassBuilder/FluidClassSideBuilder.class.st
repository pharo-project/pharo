"
I'm a builder responsible for handling the specific case of 

```
Object class >> Point class
	slots: ;
	traits: ....;
```

We want to make sure that we are not forced to use classSlots: and classTraits: 
because users will be confused. 
"
Class {
	#name : 'FluidClassSideBuilder',
	#superclass : 'FluidBuilder',
	#category : 'FluidClassBuilder-Base',
	#package : 'FluidClassBuilder',
	#tag : 'Base'
}

{ #category : 'API' }
FluidClassSideBuilder >> slots: anArray [
	"Pay attention on the class side we do not have to have classSlots: but slots:"

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	self classSlots: anArray
]

{ #category : 'API' }
FluidClassSideBuilder >> traits: aCollection [
	"Pay attention that on class side we want to write traits: and not classTraits:"

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	self classTraits: aCollection
]
