"
Dynamic variable for Spec on which of the bindings (Morphic or other) should be used as default.


Example: 
------------

```
	SpecBindings value: #MorphicAdapterBindings during: [ monUI openWithSpec ].
```
"
Class {
	#name : #SpBindings,
	#superclass : #DynamicVariable,
	#category : #'Spec2-Core-Support'
}

{ #category : #accessing }
SpBindings >> default [
	^ (self class environment at: #SpMorphicAdapterBindings) new
]
