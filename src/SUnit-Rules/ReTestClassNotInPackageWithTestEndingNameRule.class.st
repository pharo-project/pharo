"
A subclass of TestCase should be in package which name is ending with '-Tests'
"
Class {
	#name : #ReTestClassNotInPackageWithTestEndingNameRule,
	#superclass : #ReAbstractRule,
	#traits : 'ReTSUnitGroupedRule',
	#classTraits : 'ReTSUnitGroupedRule classTrait',
	#category : #'SUnit-Rules-Base'
}

{ #category : #'testing - interest' }
ReTestClassNotInPackageWithTestEndingNameRule class >> checksClass [

	^ true
]

{ #category : #running }
ReTestClassNotInPackageWithTestEndingNameRule >> basicCheck: aClass [

	^ (aClass inheritsFrom: TestCase) and: [(aClass package name asString endsWith: '-Tests') not].
]

{ #category : #accessing }
ReTestClassNotInPackageWithTestEndingNameRule >> name [
	^ 'Test class not in a package with name ending with ''-Tests'' '
]

{ #category : #accessing }
ReTestClassNotInPackageWithTestEndingNameRule >> rationale [
	^ 'A test class (subclass of TestCase) should be placed in a package with a ''-Tests'' suffix'
]

{ #category : #running }
ReTestClassNotInPackageWithTestEndingNameRule >> severity [
	^ #warning
]
