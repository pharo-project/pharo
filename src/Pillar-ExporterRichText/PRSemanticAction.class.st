"
I'm a simple class to determine what should be done when a monospaced text is displayed.
I'm like a little interpreter that will compute the entity that should be browsed.
I only work with in image entity.


```language=PharoExpression
| sem |
sem := PRSemanticAction from: 'Point class>>#setX:setY:'.
sem hasEntity
>>> true
sem browse
```
"
Class {
	#name : #PRSemanticAction,
	#superclass : #Object,
	#instVars : [
		'entity',
		'tokens'
	],
	#category : #'Pillar-ExporterRichText-Core'
}

{ #category : #'instance creation' }
PRSemanticAction class >> from: aString [ 
	^ self fromTokens: (RBScanner scanTokens: aString)
]

{ #category : #'instance creation' }
PRSemanticAction class >> fromTokens: anArray [ 
	^ self new fromTokens: anArray; yourself
]

{ #category : #'instance creation' }
PRSemanticAction >> computeEntity [
	"Try to guess what the text is:
		Point
		Point >> #setX:setY: 
		Point class >> #setX:setY: 
		#'PackageNameWithDashes'
		"
	| size |
	size := tokens size.
	size = 0 ifTrue: [ ^ self ].
	"either 'Point' for a class or #'''System-Caching''' for a package"
	size = 1 
		ifTrue: [ self getClassOrNil
						ifNil: [self getPackageOrNil ].
					^ self  ].
	
	"only 'Point class'"
	size = 2
		ifTrue: [ self getMetaClassOrNil. ^ self ].
	
	tokens second = '>>'
		ifTrue: [ self getClassOrNil. 
					 self getCompiledMethodFrom: 3  ]
		ifFalse: [ self getMetaClassOrNil.
					 self getCompiledMethodFrom: 4  ]
]

{ #category : #accessing }
PRSemanticAction >> entity [
	^ entity
]

{ #category : #'instance creation' }
PRSemanticAction >> fromTokens: anArray [
	
	anArray ifEmpty: [ ^ self].
	tokens := anArray.
	self computeEntity.
]

{ #category : #'instance creation' }
PRSemanticAction >> getClassOrNil [

	self class environment 
			at: tokens first asSymbol  
			ifPresent: [ :cl | entity := cl ]
			ifAbsent: [ ].
	^ entity
				
]

{ #category : #'instance creation' }
PRSemanticAction >> getCompiledMethodFrom: start [

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
PRSemanticAction >> getMetaClassOrNil [

	self class environment 
			at: tokens first asSymbol  
			ifPresent: [ :cl | entity := cl. 
					tokens second = 'class' 
						ifTrue: [ entity := entity class ]
						ifFalse: [ entity := nil "because we do not want Point foo" ] ]
			ifAbsent: [ ].
	^ entity
				
]

{ #category : #'instance creation' }
PRSemanticAction >> getPackageOrNil [
	entity := RPackageOrganizer default packageNamed: tokens first asString ifAbsent: [ nil ].
	^ entity
				
]

{ #category : #testing }
PRSemanticAction >> hasEntity [
	^ entity isNotNil
]

{ #category : #accessing }
PRSemanticAction >> tokens: aCol [

	tokens := aCol
]
