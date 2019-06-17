Extension { #name : #ClassRenamed }

{ #category : #'*Calypso-SystemQueries' }
ClassRenamed >> affectsMethod: aMethod [
	^self classAffected == aMethod origin
]

{ #category : #'*Calypso-SystemQueries' }
ClassRenamed >> affectsMethods [
	"methods belong to class which could be represented by tools as part of method"
	^true
]

{ #category : #'*Calypso-SystemQueries' }
ClassRenamed >> affectsMethodsDefinedInClass: aClass [

	^classRenamed == aClass
]

{ #category : #'*Calypso-SystemQueries' }
ClassRenamed >> affectsMethodsDefinedInPackage: aPackage [

	^classRenamed package == aPackage
]

{ #category : #'*Calypso-SystemQueries' }
ClassRenamed >> affectsMethodsTaggedWith: tagName [
	^classRenamed tagsForMethods includes: tagName
]

{ #category : #'*Calypso-SystemQueries' }
ClassRenamed >> affectsVariablesOf: aClass [
	"Any class has ClassVariables which are visible from both sides: instance side and class 	side.
	That's any class modification can affect variables from both class sides.
	And we do not need to distinguish it"

	aClass instanceSide = classRenamed instanceSide ifTrue: [ ^true ].
	
	^false
]

{ #category : #'*Calypso-SystemQueries' }
ClassRenamed >> canAffectResultOfMethodQuery: aMethodQuery [
	^true
]
