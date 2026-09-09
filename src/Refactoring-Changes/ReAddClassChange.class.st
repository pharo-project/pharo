"
I am a refactoring change for a class addition. Executing this change is just like an ordinary class definition by 
calling on ""my superclass"" (or ProtoObject) subclass:....instancevariable... etc.

"
Class {
	#name : 'ReAddClassChange',
	#superclass : 'ReClassChange',
	#instVars : [
		'builder',
		'definedClass'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReAddClassChange class >> make: aBlock [

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
ReAddClassChange >> asUndoOperation [

	| class |
	class := onSystemEnvironment classNamed: self changeClassName.

	^ class isBehavior
		  ifTrue: [ changeFactory addClassDefinition: [ :aBuilder | aBuilder fillFor: class ] ]
		  ifFalse: [ changeFactory removeClassNamed: self changeClassName ]
]

{ #category : 'accessing' }
ReAddClassChange >> builder [

	^ builder
]

{ #category : 'accessing' }
ReAddClassChange >> builder: aClassBuilder [

	isMeta := false.
	builder := aClassBuilder.
	className := builder name
]

{ #category : 'accessing' }
ReAddClassChange >> classInstanceVariableNames [

	^ builder classSlots collect: [ :slot | slot name ]
]

{ #category : 'accessing' }
ReAddClassChange >> classVariableNames [

	^ builder sharedVariables collect: [ :slot | slot name ]
]

{ #category : 'accessing' }
ReAddClassChange >> comment [

	^ builder comment
]

{ #category : 'accessing' }
ReAddClassChange >> definedClass [

	^ definedClass
]

{ #category : 'private' }
ReAddClassChange >> definitionClass [

	^ onSystemEnvironment classNamed:
		  (self superclassName ifNil: [ #ProtoObject ])
]

{ #category : 'private' }
ReAddClassChange >> generateChanges [

	definedClass := self class classInstaller new makeWithBuilder: self builder
]

{ #category : 'comparing' }
ReAddClassChange >> hash [

	^ builder hash
]

{ #category : 'accessing' }
ReAddClassChange >> instanceVariableNames [

	^ builder slots collect: [ :slot | slot name ]
]

{ #category : 'accessing' }
ReAddClassChange >> nameToDisplay [

	^ self changeString
]

{ #category : 'accessing' }
ReAddClassChange >> package [

	^ builder package
]

{ #category : 'accessing' }
ReAddClassChange >> sharedPoolNames [

	^ builder ifNotNil: [ builder sharedPools ]
]

{ #category : 'accessing' }
ReAddClassChange >> superclassName [

	^ builder superclassName
]

{ #category : 'accessing' }
ReAddClassChange >> tag [

	^ builder tag
]

{ #category : 'accessing' }
ReAddClassChange >> textToDisplay [

	^ 'Add class change for ' , self builder name
]

{ #category : 'accessing' }
ReAddClassChange >> traitComposition [

	^ builder traitComposition
]
