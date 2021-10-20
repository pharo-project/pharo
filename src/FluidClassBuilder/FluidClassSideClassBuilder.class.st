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
	#name : #FluidClassSideClassBuilder,
	#superclass : #FluidClassBuilder,
	#category : #'FluidClassBuilder-Base'
}
