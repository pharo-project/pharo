"
I'm a simple class to determine what should be done when a monospaced text is displayed.
I'm like a little interpreter that will compute the entity that should be browsed.
I only work with in image entity.


```language=PharoExpression
| sem |
sem := MicSemanticAction from: 'Point class>>#x:y:'.
sem hasEntity
>>> true
sem browse
```

The following entities can be referenced. 

- A class: `Point`
- A method `Point>>#degrees` as well as a class method `Point class>>x:y:`
- A package: `Microdown`

"
Class {
	#name : #MicSemanticAction,
	#superclass : #Object,
	#instVars : [
		'entity',
		'tokens'
	],
	#category : #'Microdown-RichTextComposer-Composer'
}

{ #category : #'instance creation' }
MicSemanticAction class >> from: aString [ 
	| contents |
	contents := ((ASTScanner on: (ReadStream on: aString) errorBlock: [#()]) contents).
	^ self fromTokens: (contents collect: [:e | e value])
]

{ #category : #'instance creation' }
MicSemanticAction class >> fromTokens: anArray [ 
	^ self new fromTokens: anArray; yourself
]

{ #category : #'instance creation' }
MicSemanticAction >> computeEntity [
	"Try to guess what the text is:
		Point
		Point >> #setX:setY: 
		Point class >> #setX:setY: 
		#'PackageNameWithDashes'
		"
	| size |
	size := tokens size.
	size > 4 ifTrue: [ ^ self ].
	size = 0 ifTrue: [ ^ self ].
	"either 'Point' for a class or #'''System-Caching''' for a package"
	size = 1 
		ifTrue: [ self getClassOrNil
						ifNil: [self getPackageOrNil ].
					^ self  ].
	
	"only 'Point class'"
	size = 2
		ifTrue: [ self getMetaClassOrNil. ^ self ].
	
	"Point >> #setX:setY:"
	tokens second = '>>'
		ifTrue: [ self getClassOrNil. 
					 self getCompiledMethodFrom: 3  ]
				"Point class >> #x:y:"
		ifFalse: [ tokens second = 'class'
							ifTrue: [ self getMetaClassOrNil.
					 					self getCompiledMethodFrom: 4 ]
							ifFalse: [ ^ self  ] ]
]

{ #category : #accessing }
MicSemanticAction >> entity [
	^ entity
]

{ #category : #'instance creation' }
MicSemanticAction >> fromTokens: anArray [
	
	anArray ifEmpty: [ ^ self].
	tokens := anArray.
	self computeEntity.
]

{ #category : #'instance creation' }
MicSemanticAction >> getClassOrNil [

	self class environment 
			at: tokens first asString asSymbol  
			ifPresent: [ :cl | entity := cl ]
			ifAbsent: [ ].
	^ entity
				
]

{ #category : #'instance creation' }
MicSemanticAction >> getCompiledMethodFrom: start [

	"
	| selector |
	selector := String streamContents: [ :str |
		start to: tokens size do: [ :each | str nextPutAll: (tokens at: each) ] ].
	^ selector
	No we do not want to support space in method selector!
	"
	entity ifNotNil: [  
		entity 
			compiledMethodAt: (tokens at: start) 
			ifPresent: [ :cm | entity := cm ] 
			ifAbsent: [ entity := nil ] ]
]

{ #category : #'instance creation' }
MicSemanticAction >> getMetaClassOrNil [

	self class environment 
			at: tokens first asString asSymbol  
			ifPresent: [ :cl | entity := cl. 
					tokens second = 'class' 
						ifTrue: [ entity := entity class ]
						ifFalse: [ entity := nil "because we do not want Point foo" ] ]
			ifAbsent: [ ].
	^ entity
				
]

{ #category : #'instance creation' }
MicSemanticAction >> getPackageOrNil [
	entity := RPackageOrganizer default packageNamed: tokens first asString ifAbsent: [ nil ].
	^ entity
				
]

{ #category : #testing }
MicSemanticAction >> hasEntity [
	^ entity isNotNil
]

{ #category : #accessing }
MicSemanticAction >> tokens: aCol [

	tokens := aCol
]
