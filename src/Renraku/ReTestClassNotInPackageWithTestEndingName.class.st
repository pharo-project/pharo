"
A subclass of TestCase should be in package which name is ending with '-Tests'
"
Class {
	#name : #ReTestClassNotInPackageWithTestEndingName,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing-interest' }
ReTestClassNotInPackageWithTestEndingName class >> checksClass [

	^ true
]

{ #category : #running }
ReTestClassNotInPackageWithTestEndingName >> basicCheck: aClass [

	^ (aClass inheritsFrom: TestCase) and: [(aClass package name asString endsWith: '-Tests') not].
]

{ #category : #running }
ReTestClassNotInPackageWithTestEndingName >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReTestClassNotInPackageWithTestEndingName >> name [
	^ 'Test class not in a package with name ending with ''-Tests'' '
]

{ #category : #accessing }
ReTestClassNotInPackageWithTestEndingName >> rationale [
	^ 'A test class (subclass of TestCase) should be placed in a package with a ''-Tests'' suffix'
]

{ #category : #running }
ReTestClassNotInPackageWithTestEndingName >> severity [
	^ #warning
]
