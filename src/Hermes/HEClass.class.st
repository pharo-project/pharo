Class {
	#name : #HEClass,
	#superclass : #HEMethodContainer,
	#instVars : [
		'className',
		'superclass',
		'instanceVariables',
		'classInstancevariables',
		'classVariables',
		'sharedPools',
		'layoutClass'
	],
	#category : #'Hermes-Model'
}

{ #category : #'instance creation' }
HEClass class >> for: aClass [
	^ self new
		fromClass: aClass;
		yourself
]

{ #category : #querying }
HEClass >> allSuperclassesIn: aCollection [
	^ aCollection detect:[:aSuper | aSuper className = self superclass]
		ifFound:[ :miSuper | { miSuper } , (miSuper allSuperclassesIn: aCollection) ]
		ifNone:[ #() ]
]

{ #category : #accessing }
HEClass >> classInstancevariables [
	^ classInstancevariables
]

{ #category : #accessing }
HEClass >> classInstancevariables: anObject [
	classInstancevariables := anObject
]

{ #category : #initialization }
HEClass >> className [
	^ className
]

{ #category : #accessing }
HEClass >> className: anObject [
	className := anObject
]

{ #category : #accessing }
HEClass >> classVariables [
	^ classVariables
]

{ #category : #accessing }
HEClass >> classVariables: anObject [
	classVariables := anObject
]

{ #category : #reading }
HEClass >> doReadHeaderFrom: aReader [
	className := aReader readByteSymbol.
	superclass := aReader readByteSymbol.
	instanceVariables := aReader readByteString.
	classInstancevariables := aReader readByteString.
	classVariables := aReader readByteString.
	sharedPools := aReader readByteString.
	layoutClass := aReader readByteSymbol
]

{ #category : #writing }
HEClass >> doWriteHeaderInto: aWriter [
	aWriter writeByteString: className.
	aWriter writeByteString: superclass.
	aWriter writeByteString: instanceVariables.
	aWriter writeByteString: classInstancevariables.
	aWriter writeByteString: classVariables.
	aWriter writeByteString: sharedPools.
	aWriter writeByteString: layoutClass
]

{ #category : #initialization }
HEClass >> fromClass: aClass [
	className := aClass name.
	superclass := aClass superclass ifNil: '' ifNotNil:#name.
	instanceVariables := (aClass slots collect: #name) joinUsing: ' '.
	classInstancevariables := (aClass class slots collect: #name) joinUsing: ' '.
	classVariables := aClass classPool keys joinUsing: ' '.
	sharedPools := (aClass sharedPools collect: #name) joinUsing: ' '.
	layoutClass := aClass classLayout class name.

	self doFromMethodContainer: aClass
]

{ #category : #accessing }
HEClass >> instanceVariables [
	^ instanceVariables
]

{ #category : #accessing }
HEClass >> instanceVariables: anObject [
	instanceVariables := anObject
]

{ #category : #accessing }
HEClass >> layoutClass [
	^ layoutClass
]

{ #category : #accessing }
HEClass >> layoutClass: anObject [
	layoutClass := anObject
]

{ #category : #printing }
HEClass >> printOn: aStream [
	aStream
		nextPutAll: 'HEClass ( ';
		nextPutAll: className asString;
		nextPutAll: ' )'
]

{ #category : #accessing }
HEClass >> sharedPools [
	^ sharedPools
]

{ #category : #accessing }
HEClass >> sharedPools: anObject [
	sharedPools := anObject
]

{ #category : #querying }
HEClass >> sharedPoolsIn: aCollection [
	^ ((sharedPools substrings collect: #asSymbol) collect: [ :aName | aCollection detect: [ :aClass | aClass className = aName ] ifNone: nil ]) reject: #isNil
]

{ #category : #accessing }
HEClass >> superclass [
	^ superclass
]

{ #category : #accessing }
HEClass >> superclass: anObject [
	superclass := anObject
]
