"
A filter for kernel classes
"
Class {
	#name : #KernelClassesFilter,
	#superclass : #StackFilter,
	#instVars : [
		'kernelClasses'
	],
	#category : #'Debugger-Filters-Base'
}

{ #category : #initialization }
KernelClassesFilter >> initialize [
	
	super initialize.
	kernelClasses := self kernelClassesToExclude asOrderedCollection.
	kernelClasses := kernelClasses 
		addAll: (kernelClasses collect: [:each | each class]);
		yourself
	
]

{ #category : #private }
KernelClassesFilter >> kernelClassesToExclude [
	^ {Boolean.
	True.
	False.
	BlockClosure.
	Context.
	Array.
	OrderedCollection.
	Set.
	IdentitySet.
	Dictionary.
	IdentityDictionary.
	Class.
	Metaclass.
	Behavior.
	SmallInteger.
	Float.
	Error.
	Exception.
	UndefinedObject.
	MessageSend}
]

{ #category : #testing }
KernelClassesFilter >> shouldDisplay: aContext [
	^ (kernelClasses includes: aContext receiver class) not
]
