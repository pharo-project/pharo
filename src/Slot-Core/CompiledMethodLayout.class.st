"
I am a special layout version for CompiledMethods. 
Unlike default Object layouts, CompiledMethods define a custom format integer since they mix bytes and pointer types.

See CompiledMethod for more details.
"
Class {
	#name : #CompiledMethodLayout,
	#superclass : #ObjectLayout,
	#category : #'Slot-Core-Layout'
}

{ #category : #'instance creation' }
CompiledMethodLayout class >> extending: superLayout scope: aScope host: aClass [
	^ superLayout extendCompiledMethod
		host: aClass;
		yourself
]

{ #category : #format }
CompiledMethodLayout >> instanceSpecification [
	 ^ 24
]
