"
I am a refactoring for move a method to class side.

For example, a method 

```
X >> annotatedBlockMarkup
	^ '@@'
```	

move this method to class side convert the method to:
```
X >> annotatedBlockMarkup
  ^ self class annotatedBlockMarkup
```
and create a new method in class side:

```
X class >> annotatedBlockMarkup
	^ '@@'
```
"
Class {
	#name : #RBMoveMethodToClassSideRefactoring,
	#superclass : #RBMoveMethodToClassRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #transforming }
RBMoveMethodToClassSideRefactoring >> addMethod: rbMethod to: aClass toProtocol: protocol [
	aClass addMethod: rbMethod.
	aClass compile: rbMethod source classified: {protocol}.
]

{ #category : #initialization }
RBMoveMethodToClassSideRefactoring >> method: aMethod class: aClass [
	method := aMethod.
	class := self classObjectFor: aClass.
]

{ #category : #preconditions }
RBMoveMethodToClassSideRefactoring >> preconditions [
	^(RBCondition 
		definesSelector: method selector 
		in: (self model classNamed: class name, ' class')) not
		& (RBCondition withBlock: [ class isMeta not ])
]

{ #category : #printing }
RBMoveMethodToClassSideRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' method: '.
	method storeOn: aStream.
	aStream 
		nextPutAll: ' class: ';
		nextPutAll: class name.
	aStream
		nextPutAll: ')'
]

{ #category : #transforming }
RBMoveMethodToClassSideRefactoring >> transform [
	| oldClass newClass rbMethod rbMethod2 newSource originalProtocol |
	oldClass := class.
	newClass := self model classNamed: class name, ' class'.
	rbMethod := RBClassModelFactory rbMethod for: newClass source: method sourceCode selector: method selector.
	newSource := method selector, '
	^ self class ',  method selector.
	rbMethod2 := RBClassModelFactory rbMethod for: oldClass source: newSource selector: method selector.
	originalProtocol := method protocol.
	oldClass removeMethod: method selector.
	self addMethod: rbMethod to: newClass toProtocol: originalProtocol.
	self addMethod: rbMethod2 to: oldClass toProtocol: originalProtocol.
]
