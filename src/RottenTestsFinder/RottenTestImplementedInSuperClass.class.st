"
I model a rotten test that is implemented in a superclass.

Additionally to the CompiledMethod, I hold the subclasses in which my CompiledMethod is run and for which the test is rotten.
"
Class {
	#name : 'RottenTestImplementedInSuperClass',
	#superclass : 'RottenTest',
	#instVars : [
		'subclassesInWhichTestIsRotten'
	],
	#category : 'RottenTestsFinder-Results',
	#package : 'RottenTestsFinder',
	#tag : 'Results'
}

{ #category : 'instance creation' }
RottenTestImplementedInSuperClass class >> compiledMethod: aCompiledMethod subclassInWhichTestIsRotten: aClass [
	^ self compiledMethod: aCompiledMethod subclassesInWhichTestIsRotten: (Set with: aClass)
]

{ #category : 'instance creation' }
RottenTestImplementedInSuperClass class >> compiledMethod: aCompiledMethod subclassesInWhichTestIsRotten: aCollectionOfClasses [
	^ (self compiledMethod: aCompiledMethod)
		subclassesInWhichTestIsRotten: aCollectionOfClasses;
		yourself
]

{ #category : 'visiting' }
RottenTestImplementedInSuperClass >> acceptVisitor: aRTFResultsVisitor [
	^ aRTFResultsVisitor visitRottenTestImplementedInSuperClass: self
]

{ #category : 'adding' }
RottenTestImplementedInSuperClass >> addSubclassInWhichTestIsRotten: aClass [
	subclassesInWhichTestIsRotten add: aClass
]

{ #category : 'rotten tests set' }
RottenTestImplementedInSuperClass >> addYourselfIn: aCollection [
	"If the test implemented in a super class is already present in aCollection, add a subclass to it.
	 Else, add the result to aCollection."

	self assert: self subclassesInWhichTestIsRotten size = 1.

	aCollection
		detect: [ :rottenTest |
			rottenTest isImplementedInSuperClass and: [ rottenTest superClass = self superClass ] ]
		ifFound: [ :rottenTest | rottenTest addSubclassInWhichTestIsRotten: self subclassesInWhichTestIsRotten anyOne ]
		ifNone: [ aCollection add: self ]
]

{ #category : 'testing' }
RottenTestImplementedInSuperClass >> isImplementedInSuperClass [
	^ true
]

{ #category : 'accessing' }
RottenTestImplementedInSuperClass >> subclassesInWhichTestIsRotten [
	^ subclassesInWhichTestIsRotten
]

{ #category : 'accessing' }
RottenTestImplementedInSuperClass >> subclassesInWhichTestIsRotten: anObject [
	subclassesInWhichTestIsRotten := anObject
]

{ #category : 'accessing' }
RottenTestImplementedInSuperClass >> superClass [
	^ self compiledMethod class
]
