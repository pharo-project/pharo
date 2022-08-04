"
I model a rotten test that is implemented in a Trait.

Additionally to the CompiledMethod, I hold the classes using the Trait that defined my CompiledMethod and for which the test is rotten.
"
Class {
	#name : #RottenTestImplementedInTrait,
	#superclass : #RottenTest,
	#instVars : [
		'traitUsersInWhichTestIsRotten'
	],
	#category : #'RottenTestsFinder-Results'
}

{ #category : #'instance creation' }
RottenTestImplementedInTrait class >> compiledMethod: aCompiledMethod [
	^ (super compiledMethod: aCompiledMethod)
			traitUsersInWhichTestIsRotten: (Set with: aCompiledMethod origin);
			yourself
]

{ #category : #visiting }
RottenTestImplementedInTrait >> acceptVisitor: aRTFResultsVisitor [
	^ aRTFResultsVisitor visitRottenTestImplementedInTrait: self
]

{ #category : #adding }
RottenTestImplementedInTrait >> addClassInWhichTestIsRotten: aClass [
	traitUsersInWhichTestIsRotten add: aClass
]

{ #category : #accessing }
RottenTestImplementedInTrait >> allRottenCompiledMethods [
	^ self traitUsersInWhichTestIsRotten collect: [ :class |
		class >> self compiledMethod selector ]
]

{ #category : #accessing }
RottenTestImplementedInTrait >> compiledMethod [
	"Returns the CompiledMethod implemented in by the Trait."
	^ super compiledMethod origin >> super compiledMethod selector
]

{ #category : #testing }
RottenTestImplementedInTrait >> isImplementedInTrait [
	^ true
]

{ #category : #accessing }
RottenTestImplementedInTrait >> trait [
	^ self compiledMethod origin
]

{ #category : #accessing }
RottenTestImplementedInTrait >> traitUsersInWhichTestIsRotten [
	^ traitUsersInWhichTestIsRotten
]

{ #category : #accessing }
RottenTestImplementedInTrait >> traitUsersInWhichTestIsRotten: anObject [
	traitUsersInWhichTestIsRotten := anObject
]
