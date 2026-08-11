Extension { #name : 'MethodRepackaged' }

{ #category : '*Calypso-SystemQueries' }
MethodRepackaged >> affectsClassExtension [
	^true
]

{ #category : '*Calypso-SystemQueries' }
MethodRepackaged >> affectsClassesExtendedInPackage: aPackage [
	^(super affectsClassesExtendedInPackage: aPackage)
		or: [ oldPackage = aPackage ]
]

{ #category : '*Calypso-SystemQueries' }
MethodRepackaged >> affectsMethodsDefinedInPackage: aPackage [

	^ (super affectsMethodsDefinedInPackage: aPackage) or: [ self oldPackage = aPackage ]
]

{ #category : '*Calypso-SystemQueries' }
MethodRepackaged >> affectsPackage: aPackage [

	^ self newPackage == aPackage or: [ self oldPackage == aPackage ]
]
