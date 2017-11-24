"
A PDCompositeDependency is a composite dependency. A composite pattern is using.

Actually in all case this dependency contains the dependencies between two packages and the instance variable dependencies contains all the type of dependencies (Extension, Inheritance, Reference, Trait).
"
Class {
	#name : #DACompositeDependency,
	#superclass : #DAPackageDependency,
	#instVars : [
		'dependencies'
	],
	#category : #'Tool-DependencyAnalyser-Core'
}

{ #category : #adding }
DACompositeDependency >> add: aDependency [
	dependencies add: aDependency
]

{ #category : #testing }
DACompositeDependency >> containsOnlyExtensionDependency [
	(self dependencies isEmpty ) ifTrue: [ ^ false ].
	 ^ self dependencies allSatisfy: [ :dependency | dependency isExtensionDependency ]
]

{ #category : #accessing }
DACompositeDependency >> dependencies [
	^ dependencies
]

{ #category : #accessing }
DACompositeDependency >> dependenciesFrom: aClass [
	
	^ dependencies select: [ :reference | reference theClass = aClass ].
]

{ #category : #testing }
DACompositeDependency >> dependenciesWithoutExtensionDependency [
	^ dependencies reject: [ :dependency | dependency isExtensionDependency ].
]

{ #category : #accessing }
DACompositeDependency >> extensionDependencies [
 	^ dependencies select: [ :each | each isExtensionDependency ]
]

{ #category : #accessing }
DACompositeDependency >> extensionDependenciesSize [
	^ self dependencies
		detectSum: [ :dependency | 
			dependency isExtensionDependency
				ifTrue: [ 1 ]
				ifFalse: [ 0 ] ]
]

{ #category : #initialization }
DACompositeDependency >> initializeFrom: aSourcePackage to: aTargetPackage [

	super initializeFrom: aSourcePackage to: aTargetPackage.
	dependencies := OrderedCollection new
]

{ #category : #accessing }
DACompositeDependency >> numberOfDependencies [
	^ dependencies size
]

{ #category : #printing }
DACompositeDependency >> printReasonOn: aStream [
	aStream nextPutAll: dependencies size asString; nextPutAll: ' dependencies: '.
	dependencies
		do: [ :each | each printReasonOn: aStream ]
		separatedBy: [ aStream nextPutAll: ', ' ]
]

{ #category : #printing }
DACompositeDependency >> printShortReasonOn: aStream [
	aStream nextPutAll: dependencies size asString
]

{ #category : #accessing }
DACompositeDependency >> referenceDependencies [
 	^ dependencies select: [ :each | each isReferenceDependency ]
]

{ #category : #accessing }
DACompositeDependency >> referenceDependenciesFrom: aClass [
	| references |
	references := self referenceDependencies.
	
	^ references select: [ :reference | reference theClass = aClass ].
]
