"
I model Class Variables and therefore Pool Variables, too. I am stored as a binding in the classPool of the class defining me.

The compiler forwards bytecode generation to me for accessing the variable.

You can subclass me and implement #read and #write: for creating special kinds of globals that can be used as special class Variables (similar to special Slots).

"
Class {
	#name : #ClassVariable,
	#superclass : #LiteralVariable,
	#category : #'Slot-Core-Variables'
}

{ #category : #printing }
ClassVariable >> definitionString [
	"non special globals are defined by the symbol"
	^self isSpecial
		ifTrue: [super definitionString]
		ifFalse: [self name printString]

]

{ #category : #'code generation' }
ClassVariable >> emitStore: methodBuilder [

	methodBuilder storeIntoLiteralVariable: self.


]

{ #category : #'code generation' }
ClassVariable >> emitValue: methodBuilder [

	methodBuilder pushLiteralVariable: self.
]

{ #category : #testing }
ClassVariable >> isClassVariable [
	^true
]

{ #category : #testing }
ClassVariable >> isSpecial [
	"I just a class var Note: my subclasses are special"
	^(self class = ClassVariable) not
]
