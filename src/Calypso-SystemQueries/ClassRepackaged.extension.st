Extension { #name : #ClassRepackaged }

{ #category : #'*Calypso-SystemQueries' }
ClassRepackaged >> affectsMethods [
	"methods belong to class which could be represented by tools as part of method"
	^true
]

{ #category : #'*Calypso-SystemQueries' }
ClassRepackaged >> affectsMethodsDefinedInClass: aClass [

	^classRepackaged == aClass
]

{ #category : #'*Calypso-SystemQueries' }
ClassRepackaged >> affectsMethodsDefinedInPackage: aPackage [

	^oldPackage == aPackage or: [ newPackage == aPackage ]
]

{ #category : #'*Calypso-SystemQueries' }
ClassRepackaged >> affectsMethodsTaggedWith: tagName [
	^classRepackaged tagsForMethods includes: tagName
]

{ #category : #'*Calypso-SystemQueries' }
ClassRepackaged >> canAffectResultOfMethodQuery: aMethodQuery [

	^true
]
