"
I'm a builder responsible for handling the specific case of 

```
Trait >> TPoint classTrait
	slots: ;
	traits: ....;
```

We want to make sure that we are not forced to use classSlots: and classTraits: 
because users will be confused. 
"
Class {
	#name : #FluidClassSideTraitBuilder,
	#superclass : #FluidTraitBuilder,
	#category : #'FluidClassBuilder-Base'
}
