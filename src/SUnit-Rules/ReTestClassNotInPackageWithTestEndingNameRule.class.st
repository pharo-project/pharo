"
A subclass of TestCase should be in package which name is ending with '-Tests'
"
Class {
	#name : 'ReTestClassNotInPackageWithTestEndingNameRule',
	#superclass : 'ReAbstractRule',
	#traits : 'ReTSUnitGroupedRule',
	#classTraits : 'ReTSUnitGroupedRule classTrait',
	#category : 'SUnit-Rules-SUnit',
	#package : 'SUnit-Rules',
	#tag : 'SUnit'
}

{ #category : 'testing - interest' }
ReTestClassNotInPackageWithTestEndingNameRule class >> checksClass [

	^ true
]

{ #category : 'accessing' }
ReTestClassNotInPackageWithTestEndingNameRule class >> group [

	^ 'SUnit'
]

{ #category : 'accessing' }
ReTestClassNotInPackageWithTestEndingNameRule class >> rationale [
	^ 'A test class (subclass of TestCase) should be placed in a package with a ''-Tests'' suffix'
]

{ #category : 'accessing' }
ReTestClassNotInPackageWithTestEndingNameRule class >> ruleName [
	^ 'Test class not in a package with name ending with ''-Tests'' '
]

{ #category : 'running' }
ReTestClassNotInPackageWithTestEndingNameRule >> basicCheck: aClass [

	^ (aClass inheritsFrom: TestCase) and: [(aClass package name asString endsWith: '-Tests') not]
]
