"
I detect the changes to a given Class.
My subclasses implements 

#compareClass: oldClass with: aBuilder to calculate the changes performed to the class.

I am responsible of adding the changes to the builder..

Check the initialization of the builder in the ShiftClassBuilder and in the ShDefaultBuilderEnhancer to see when the comparers are registered in the Shift class builder.
"
Class {
	#name : 'ShAbstractChangeDetector',
	#superclass : 'Object',
	#instVars : [
		'builder'
	],
	#category : 'Shift-Changes',
	#package : 'Shift-Changes'
}

{ #category : 'comparing' }
ShAbstractChangeDetector class >> builder: aBuilder [

	^ self new
		  builder: aBuilder;
		  yourself
]

{ #category : 'comparing' }
ShAbstractChangeDetector class >> compareClass: oldClass with: aBuilder [

	(self builder: aBuilder) compareClass do: [ :change | aBuilder addChange: change ]
]

{ #category : 'testing' }
ShAbstractChangeDetector class >> isAbstract [

	^ self == ShAbstractChangeDetector
]

{ #category : 'accessing' }
ShAbstractChangeDetector >> builder [
	^ builder
]

{ #category : 'accessing' }
ShAbstractChangeDetector >> builder: anObject [
	builder := anObject
]

{ #category : 'comparing' }
ShAbstractChangeDetector >> compareClass [
	^ self subclassResponsibility
]
