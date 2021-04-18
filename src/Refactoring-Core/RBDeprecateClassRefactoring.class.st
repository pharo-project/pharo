"
I am a refactoring to mark a class as deprecated.

I am rename deprecated class references with the new class, then I add a subclass of deprecated class and finally I add #isDeprecated method in subclass created.

className
	- is a symbol with class name, it's deprecated
newClass
	- is a class name which will be used instead of a deprecated one
	
Example
--------

Script:

```
(RBDeprecateClassRefactoring deprecated: #MyClassAA in: #MyClassAC) execute
```

Before refactoring:

```
Object subclass: #MyClassAA
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Example'

Object subclass: #MyClassAC
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Example'
	
Object subclass: #OtherClass
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Example'
	
OtherClass >> referenceToClass
	^ MyClassAA new

OtherClass >> symbolReference
	#MyClassAA
```

After refactoring:
```
Object subclass: #MyClassAA
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Example'
	
MyClassAA subclass: #MyClassAA1
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''Example''

Object subclass: #MyClassAC
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Example'
	
Object subclass: #OtherClass
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Example'

MyClassAA1 class >> isDeprecated
	"" Uses MyClassAC instead of MyClassAA""
	^ true

OtherClass >> referenceToClass
	^ MyClassAC new

OtherClass >> symbolReference
	#MyClassAC
```

"
Class {
	#name : #RBDeprecateClassRefactoring,
	#superclass : #RBClassRefactoring,
	#instVars : [
		'class',
		'newClass'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #transforming }
RBDeprecateClassRefactoring class >> deprecated: aClass in: newClassName [
	^ self new 
		deprecated: aClass
		in: newClassName
]

{ #category : #transforming }
RBDeprecateClassRefactoring class >> model: aRBSmalltalk deprecated: aClass in: newClassName [
	^ self new
		model: aRBSmalltalk; 
		deprecated: aClass
		in: newClassName
]

{ #category : #transforming }
RBDeprecateClassRefactoring >> addDeprecatedMethodIn: newSubclass [
	| subclass |
	subclass := (self model classNamed: newSubclass) classSide.
	subclass compile: 'isDeprecated
		" Uses ', newClass, ' instead of ', className,'"
		^ true' classified: {'testing'}
]

{ #category : #transforming }
RBDeprecateClassRefactoring >> createSubclass [ 
	| newSubclass refactoring |
	newSubclass := self subclassNameFor: className.
	refactoring :=  RBAddClassRefactoring
		model: self model
		addClass: newSubclass
		superclass: class
		subclasses: class subclasses
		category: class category.
	self performCompositeRefactoring: refactoring.
	^ newSubclass
]

{ #category : #transforming }
RBDeprecateClassRefactoring >> deprecated: aClass in: newClassName [
	className := aClass.
	class := self model classNamed: aClass.
	newClass := newClassName.
]

{ #category : #transforming }
RBDeprecateClassRefactoring >> parseTreeRewriter [
	^ (self parseTreeRewriterClass replaceLiteral: className with: newClass)
				replace: className with: newClass;
				replaceArgument: newClass
					withValueFrom: 
						[:aNode | 
						self 
							refactoringFailure: newClass , ' already exists within the reference scope'];
				yourself
]

{ #category : #preconditions }
RBDeprecateClassRefactoring >> preconditions [
	^((RBCondition withBlock: [class notNil and: [class isMeta not]]) 
			& (RBCondition isValidClassName: newClass) 
			& (RBCondition isGlobal: newClass in: self model)) |
			(RBCondition withBlock: [ self refactoringFailure: newClass, ' is not a valid class name'])
]

{ #category : #transforming }
RBDeprecateClassRefactoring >> renameReferences [ 
	self convertAllReferencesToClass: class using: self parseTreeRewriter.
]

{ #category : #printing }
RBDeprecateClassRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' deprecated: #';
		nextPutAll: className;
		nextPutAll: ' in: #';
		nextPutAll: newClass;
		nextPut: $)
]

{ #category : #transforming }
RBDeprecateClassRefactoring >> subclassNameFor: symbol [
	| counter copyName |
	counter := 1.
	copyName := symbol, counter asString.
	[ self model includesGlobal: copyName ] 
		whileTrue: [ counter := counter + 1.
			copyName := symbol, counter asString ].
	^ copyName
]

{ #category : #transforming }
RBDeprecateClassRefactoring >> transform [ 
	| newSubclass |
	self renameReferences.
	newSubclass := self createSubclass.
	self addDeprecatedMethodIn: newSubclass.
]
