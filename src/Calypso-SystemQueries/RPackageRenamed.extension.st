Extension { #name : #RPackageRenamed }

{ #category : #'*Calypso-SystemQueries' }
RPackageRenamed >> affectsClass: aClass [
	^package == aClass package 
]

{ #category : #'*Calypso-SystemQueries' }
RPackageRenamed >> affectsMethod: aMethod [
	^package == aMethod package
]

{ #category : #'*Calypso-SystemQueries' }
RPackageRenamed >> affectsMethods [
	"methods belong to package which could be represented by tools as part of method"
	^true
]

{ #category : #'*Calypso-SystemQueries' }
RPackageRenamed >> affectsMethodsDefinedInClass: aClass [

	^(package definesClass: aClass)
		or: [ package extendsClass: aClass ]
]

{ #category : #'*Calypso-SystemQueries' }
RPackageRenamed >> affectsMethodsDefinedInPackage: aPackage [

	^package == aPackage
]

{ #category : #'*Calypso-SystemQueries' }
RPackageRenamed >> canAffectResultOfMethodQuery: aMethodQuery [
	^true
]
