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
	
	realInstanceSideClass := self class environment at: instanceSideClassName ifAbsent: [ ^ nil ].
	^ self onClassSide
		ifFalse: [ realInstanceSideClass methodDict at: selector ifAbsent: nil ] 
		ifTrue: [ realInstanceSideClass classSide methodDict at: selector ifAbsent: nil ]
		
]

{ #category : #accessing }
MethodReference >> instanceSideClassName [
	^ instanceSideClassName
]

{ #category : #accessing }
MethodReference >> instanceSideClassName: anObject [
	instanceSideClassName := anObject
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
MethodReference >> realClass: aClass selector: aString [

	self instanceSideClassName: (aClass isClassSide
		ifTrue: [ aClass instanceSide name ]
		ifFalse: [ aClass name ]) asSymbol.
		
	self onClassSide: aClass isClassSide.
	
	self selector: aString asSymbol
]

{ #category : #accessing }
MethodReference >> selector [
	^ selector
]

{ #category : #accessing }
MethodReference >> selector: anObject [
	selector := anObject
]
