"
I am example of inheritable process specific variable.
I implement isInheritable on class side to return true when I enabled (I become enable only in tests because existance of inheritable variables effect process instantiation performance)
"
Class {
	#name : #TestInheritableVariable,
	#superclass : #ProcessLocalVariable,
	#classInstVars : [
		'enabled'
	],
	#category : #'Kernel-Tests-Processes'
}

{ #category : #controlling }
TestInheritableVariable class >> disable [
	enabled := false.
	Process updateInheritableKeys
]

{ #category : #controlling }
TestInheritableVariable class >> enable [
	enabled := true.
	Process updateInheritableKeys
]

{ #category : #testing }
TestInheritableVariable class >> enabled [
	^enabled ifNil: [ enabled := false ]
]

{ #category : #testing }
TestInheritableVariable class >> isInheritable [
	^self enabled
]
