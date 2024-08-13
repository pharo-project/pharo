"
I am a refactoring change for a class addition. Executing this change is just like an ordinary class definition by 
calling on ""my superclass"" (or ProtoObject) subclass:....instancevariable... etc.

"
Class {
	#name : 'RBAddClassChange',
	#superclass : 'RBRefactoryClassChange',
	#instVars : [
		'builder',
		'definedClass'
	],
	#category : 'Refactoring-Changes-Changes-Classes',
	#package : 'Refactoring-Changes',
	#tag : 'Changes-Classes'
}

{ #category : 'instance creation' }
RBAddClassChange class >> make: aBlock [

	| builder |
	builder := ShiftClassBuilder new.

	[ aBlock value: builder ]
		on: Warning
		do: [ :e | e resume ].

	^ self new
		  builder: builder;
		  yourself
]

{ #category : 'converting' }
RBAddClassChange >> asUndoOperation [

	| class |
	class := onSystemEnvironment classNamed: self changeClassName.

	^ class isBehavior
		  ifTrue: [ changeFactory addClassDefinition: [ :aBuilder | aBuilder fillFor: class ] ]
		  ifFalse: [ changeFactory removeClassNamed: self changeClassName ]
]

{ #category : 'accessing' }
RBAddClassChange >> builder [

	^ builder
]

{ #category : 'accessing' }
RBAddClassChange >> builder: aClassBuilder [

	isMeta := false.
	builder := aClassBuilder.
	className := builder name
]

{ #category : 'accessing' }
RBAddClassChange >> classInstanceVariableNames [

	^ builder classSlots collect: [ :slot | slot name ]
]

{ #category : 'accessing' }
RBAddClassChange >> classVariableNames [

	^ builder sharedVariables collect: [ :slot | slot name ]
]

{ #category : 'accessing' }
RBAddClassChange >> comment [

	^ builder comment
]

{ #category : 'accessing' }
RBAddClassChange >> definedClass [

	^ definedClass
]

{ #category : 'private' }
RBAddClassChange >> definitionClass [

	^ onSystemEnvironment classNamed:
		  (self superclassName ifNil: [ #ProtoObject ])
]

{ #category : 'private' }
RBAddClassChange >> generateChanges [

	definedClass := self class classInstaller new makeWithBuilder: self builder
]

{ #category : 'comparing' }
RBAddClassChange >> hash [

	^ builder hash
]

{ #category : 'accessing' }
RBAddClassChange >> instanceVariableNames [

	^ builder slots collect: [ :slot | slot name ]
]

{ #category : 'accessing' }
RBAddClassChange >> nameToDisplay [

	^ self changeString
]

{ #category : 'accessing' }
RBAddClassChange >> package [

	^ builder package
]

{ #category : 'accessing' }
RBAddClassChange >> sharedPoolNames [

	^ builder ifNotNil: [ builder sharedPools ]
]

{ #category : 'accessing' }
RBAddClassChange >> superclassName [

	^ builder superclassName
]

{ #category : 'accessing' }
RBAddClassChange >> tag [

	^ builder tag
]

{ #category : 'accessing' }
RBAddClassChange >> textToDisplay [

	^ 'Add class change for ' , self builder name
]
