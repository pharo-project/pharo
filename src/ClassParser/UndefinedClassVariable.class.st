"
When loading a class where a ClassVariable subclass is used that is not present in the System, we need to model this variable somehow so we can fix it as soon as the class is loaded.

The UndefinedClassVariable ignores reads and writes (it returns nil). But on access, it checks if the class implementing the Variable has been loaded and if yes, it rebuilds the class definition.
"
Class {
	#name : 'UndefinedClassVariable',
	#superclass : 'LiteralVariable',
	#instVars : [
		'ast',
		'classIsRebuild'
	],
	#category : 'ClassParser-Utils',
	#package : 'ClassParser',
	#tag : 'Utils'
}

{ #category : 'instance creation' }
UndefinedClassVariable class >> named: aName ast: aSlotClassName [
	^ (self named: aName) ast: aSlotClassName
]

{ #category : 'accessing' }
UndefinedClassVariable >> ast: aMessageNode [
	classIsRebuild := false.
	ast := aMessageNode
]

{ #category : 'private' }
UndefinedClassVariable >> checkClassRebuild [
	"break the recursion while rebuilding"
	classIsRebuild ifTrue: [ ^ self].
	(self definingClass environment hasClassNamed: self variableClassName) ifFalse: [ ^ self ].
	classIsRebuild := true.
	"we rebuild the class, this triggers instance migration"
	(self definingClass compiler evaluate: self definingClass definitionString) install.
	"recompile all methods that access me to generat code for the loaded definition"
	self usingMethods do: [:each | each recompile]
]

{ #category : 'printing' }
UndefinedClassVariable >> printOn: aStream [
	"we print as the definition that could not be loaded"
	aStream nextPutAll: ast formattedCode
]

{ #category : 'meta-object-protocol' }
UndefinedClassVariable >> read [
	"Undeclared Class Variables read nil always, but check if they can repair the class"
	 self checkClassRebuild.
	^ nil
]

{ #category : 'accessing' }
UndefinedClassVariable >> variableClassName [
	^ast arguments first variable name
]

{ #category : 'meta-object-protocol' }
UndefinedClassVariable >> write: aValue [
	"Undeclared Class Variables ignore writes, but check if they can repair the class"
	 self checkClassRebuild.
	^ aValue
]
