"
Dynamic variable for Spec on which of the bindings (Morphic or other) should be used as default.


Example: 
------------

```
	SpecBindings value: #MorphicAdapterBindings during: [ monUI openWithSpec ].
```
"
Class {
	#name : #SpecBindings,
	#superclass : #DynamicVariable,
	#category : #'Spec-Core-Support'
}

{ #category : #accessing }
SpecBindings >> default [
	^ self class environment at: #MorphicAdapterBindings
]
