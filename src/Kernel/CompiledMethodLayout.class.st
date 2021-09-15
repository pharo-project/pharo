"
I am a special layout version for CompiledMethods. 
Unlike default Object layouts, CompiledMethods define a custom format integer since they mix bytes and pointer types.

See CompiledMethod for more details.
"
Class {
	#name : #CompiledMethodLayout,
	#superclass : #ObjectLayout,
	#category : #'Kernel-Layout'
}

{ #category : #'instance creation' }
CompiledMethodLayout class >> extending: superLayout scope: aScope host: aClass [
	^ superLayout extendCompiledMethod
		host: aClass;
		yourself
]

{ #category : #description }
CompiledMethodLayout class >> mcTypeSymbol [ 
	^ #compiledMethod 
]

{ #category : #description }
CompiledMethodLayout class >> subclassDefiningSymbol [
	"there is no way to define classes of this layoyt, the system shows them as variable classes"
	^#variableByteSubclass:
]

{ #category : #extending }
CompiledMethodLayout >> extend [
	self error: 'CompiledMethodLayout can not be extendend'
]

{ #category : #format }
CompiledMethodLayout >> instanceSpecification [
	 ^ 24
]
