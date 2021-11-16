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

{ #category : #accessing }
FluidClassSideTraitBuilder >> slots: anArray [
	"pay attention on the class side we do not have to have classSlots: but slots:"
	
	classSlotsToBuild := anArray collect: [ :each | each asSlot ]
]

{ #category : #accessing }
FluidClassSideTraitBuilder >> traits: aCollection [
	"pay attention that on class side we want to write traits: and not classTraits:"
	
	classTraitsToBuild := aCollection asTraitComposition 
]
