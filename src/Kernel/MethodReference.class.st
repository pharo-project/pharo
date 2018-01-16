"
A MethodReference is is a lightweight proxy for a CompiledMethod.  Has methods for pointed to the CompileMethod
"
Class {
	#name : #MethodReference,
	#superclass : #Object,
	#instVars : [
		'instanceSideClassName',
		'onClassSide',
		'selector'
	],
	#category : #'Kernel-Methods'
}

{ #category : #'instance creation' }
MethodReference class >> realClass: aClass selector: aString [

	^ self new 
		realClass: aClass selector: aString;
		yourself
]

{ #category : #accessing }
MethodReference >> compiledMethod [

	| realInstanceSideClass |
	
	^ (self realClassOrNil ifNil: [ ^ nil ])
		 methodDict at: selector ifAbsent: nil
	
]

{ #category : #accessing }
MethodReference >> instanceSideClassName [
	^ instanceSideClassName
]

{ #category : #accessing }
MethodReference >> instanceSideClassName: anObject [
	instanceSideClassName := anObject
]

{ #category : #testing }
MethodReference >> isLocalSelector [

	| realInstanceSideClass |
	
	realInstanceSideClass := self realInstanceSideClassOrNil.
	realInstanceSideClass ifNil: [ ^ false ].

	^ realInstanceSideClass includesLocalSelector: self selector
]

{ #category : #testing }
MethodReference >> isValid [
	"Answer whether the receiver represents a current selector or Comment"

	selector isDoIt ifTrue: [^ false].
	^ self compiledMethod notNil.
]

{ #category : #accessing }
MethodReference >> onClassSide [
	^ onClassSide
]

{ #category : #accessing }
MethodReference >> onClassSide: anObject [
	onClassSide := anObject
]

{ #category : #accessing }
MethodReference >> realClass [

	^ self realClassOrNil 
]

{ #category : #accessing }
MethodReference >> realClass: aClass selector: aString [

	self instanceSideClassName: (aClass isClassSide
		ifTrue: [ aClass instanceSide name ]
		ifFalse: [ aClass name ]) asSymbol.
		
	self onClassSide: aClass isClassSide.
	
	self selector: aString asSymbol
]

{ #category : #accessing }
MethodReference >> realClassOrNil [

	| realInstanceSideClass |
	
	realInstanceSideClass := self realInstanceSideClassOrNil.
	realInstanceSideClass ifNil: [ ^ nil ].
	
	^ self onClassSide
		ifFalse: [ realInstanceSideClass ] 
		ifTrue: [ realInstanceSideClass classSide ]
		
]

{ #category : #accessing }
MethodReference >> realInstanceSideClassOrNil [

	^ self class environment at: instanceSideClassName ifAbsent: [ nil ].

]

{ #category : #accessing }
MethodReference >> selector [
	^ selector
]

{ #category : #accessing }
MethodReference >> selector: anObject [
	selector := anObject
]
