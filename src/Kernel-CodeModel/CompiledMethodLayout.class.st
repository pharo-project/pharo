"
I am a special layout version for CompiledMethods. 
Unlike default Object layouts, CompiledMethods define a custom format integer since they mix bytes and pointer types.

See CompiledMethod for more details.
"
Class {
	#name : 'CompiledMethodLayout',
	#superclass : 'ObjectLayout',
	#category : 'Kernel-CodeModel-Layout',
	#package : 'Kernel-CodeModel',
	#tag : 'Layout'
}

{ #category : 'instance creation' }
CompiledMethodLayout class >> extending: superLayout scope: aScope host: aClass [
	^ superLayout extendCompiledMethod
		host: aClass;
		yourself
]

{ #category : 'description' }
CompiledMethodLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton, usage makes no sense for anything
	else!"
	"there is no way to define classes of this Layput, the system shows them as variable classes"
	^#variableByteSubclass:
]

{ #category : 'extending' }
CompiledMethodLayout >> extend [
	self error: 'CompiledMethodLayout can not be extendend'
]

{ #category : 'format' }
CompiledMethodLayout >> instanceSpecification [
	 ^ 24
]
