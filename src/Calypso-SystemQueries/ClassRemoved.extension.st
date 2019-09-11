Extension { #name : #ClassRemoved }

{ #category : #'*Calypso-SystemQueries' }
ClassRemoved >> affectsMethod: aMethod [
	^classRemoved == aMethod origin
]

{ #category : #'*Calypso-SystemQueries' }
ClassRemoved >> affectsMethods [
	"methods are removed together with removed class"
	^true
]

{ #category : #'*Calypso-SystemQueries' }
ClassRemoved >> affectsMethodsDefinedInClass: aClass [

	^classRemoved == aClass
]

{ #category : #'*Calypso-SystemQueries' }
ClassRemoved >> affectsMethodsDefinedInPackage: aPackage [

	^ownerPackage == aPackage
]

{ #category : #'*Calypso-SystemQueries' }
ClassRemoved >> affectsMethodsTaggedWith: tagName [
	^classRemoved tagsForMethods includes: tagName
]

{ #category : #'*Calypso-SystemQueries' }
ClassRemoved >> canAffectResultOfMethodQuery: aMethodQuery [
	^true
]
