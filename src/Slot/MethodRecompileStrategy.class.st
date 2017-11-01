"
I am simple update strategy that recompiles all methods.
"
Class {
	#name : #MethodRecompileStrategy,
	#superclass : #AbstractMethodUpdateStrategy,
	#category : 'Slot-ClassBuilder'
}

{ #category : #updating }
MethodRecompileStrategy >> transform: oldClass to: newClass using: aMethodModification [
	newClass compileAllFrom: oldClass.
	
]

{ #category : #updating }
MethodRecompileStrategy >> updateClassLiteralKeysIn: aClass [ 
	aClass isMeta ifTrue: [ ^ self ].
	
	aClass methodsDo: [ :method | 
		method classBinding: (aClass environment bindingOf: aClass name) ]
]
