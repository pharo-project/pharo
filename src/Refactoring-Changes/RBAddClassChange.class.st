"
I am a refactoring change for a class addition. Executing this change is just like an ordinary class definition by 
calling on ""my superclass"" (or ProtoObject) subclass:....instancevariable... etc.

"
Class {
	#name : 'RBAddClassChange',
	#superclass : 'RBRefactoryDefinitionChange',
	#instVars : [
		'superclassName',
		'instanceVariableNames',
		'classVariableNames',
		'poolDictionaryNames',
		'category'
	],
	#category : 'Refactoring-Changes-Changes-Classes',
	#package : 'Refactoring-Changes',
	#tag : 'Changes-Classes'
}

{ #category : 'converting' }
RBAddClassChange >> asUndoOperation [

	| class |
	class := onSystemDictionary classNamed: self changeClassName.

	^ class isBehavior
		  ifTrue: [ changeFactory addClassDefinition: [ :aBuilder | aBuilder fillFor: class ] ]
		  ifFalse: [ changeFactory removeClassNamed: self changeClassName ]
]

{ #category : 'accessing' }
RBAddClassChange >> category [
	self flag: #refacto. "remove var + ifNotNil:"
	^ category ifNil: [ builder ifNotNil: [ builder category ] ]
]

{ #category : 'accessing' }
RBAddClassChange >> classInstanceVariableNames [

	self flag: #refacto. "remove var + ifNotNil:"
	^ builder ifNotNil: [ builder classSlots collect: [ :slot | slot name ] ]
]

{ #category : 'accessing' }
RBAddClassChange >> classVariableNames [

	self flag: #refacto. "remove var + ifNotNil:"
	^ classVariableNames ifNil: [ builder ifNotNil: [ builder sharedVariables collect: [ :slot | slot name ] ] ]
]

{ #category : 'accessing' }
RBAddClassChange >> comment [

	self flag: #refacto. "remove var + ifNotNil:"
	^ builder ifNotNil: [ builder comment ]
]

{ #category : 'private' }
RBAddClassChange >> definitionClass [

	^ onSystemDictionary classNamed:
		  (self superclassName ifNil: [ #ProtoObject ])
]

{ #category : 'accessing' }
RBAddClassChange >> instanceVariableNames [
	
	self flag: #refacto. "remove var + ifNotNil:"
	^ instanceVariableNames ifNil: [ builder ifNotNil: [ builder slots collect: [ :slot | slot name ] ] ]
]

{ #category : 'accessing' }
RBAddClassChange >> nameToDisplay [

	^ self changeString
]

{ #category : 'accessing' }
RBAddClassChange >> sharedPoolNames [

	self flag: #refacto. "remove var + ifNotNil:"
	^ poolDictionaryNames ifNil: [ builder ifNotNil: [ builder sharedPools ] ]
]

{ #category : 'accessing' }
RBAddClassChange >> superclassName [

	self flag: #refacto. "remove var + ifNotNil:"
	^ superclassName ifNil: [ builder ifNotNil: [ builder superclassName ] ]
]

{ #category : 'accessing' }
RBAddClassChange >> textToDisplay [

	^ 'Add class change for ' , self builder name
]
