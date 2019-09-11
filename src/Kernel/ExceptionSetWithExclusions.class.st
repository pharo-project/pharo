"
I am ExceptionSetWithExclusions, an ExceptionSet that explicitely does not handle a number of exclusion Exceptions.
"
Class {
	#name : #ExceptionSetWithExclusions,
	#superclass : #ExceptionSet,
	#instVars : [
		'exclusions'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #exceptionselector }
ExceptionSetWithExclusions >> - exception [
	"Add exception as an exclusion to me.
	I will explicitly not handle my exclusion exceptions."
	
	self addExclusion: exception
]

{ #category : #private }
ExceptionSetWithExclusions >> addExclusion: exception [
	"Add exception as an exclusion to me.
	I will explicitly not handle my exclusion exceptions."
	
	^ exclusions add: exception
]

{ #category : #exceptionselector }
ExceptionSetWithExclusions >> handles: exception [
	"Return true when I will handled exception.
	I extend my superclass behavior by explicitly 
	not handling a number of exclusion exceptions."

	^ (super handles: exception) 
			and: [ exclusions noneSatisfy: [ :each | each handles: exception ] ]
]

{ #category : #initialization }
ExceptionSetWithExclusions >> initialize [

	super initialize.
	exclusions := OrderedCollection new
]
