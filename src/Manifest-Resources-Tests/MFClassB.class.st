"
I'm a dummy class for test purposes  
"
Class {
	#name : #MFClassB,
	#superclass : #Object,
	#category : #'Manifest-Resources-Tests'
}

{ #category : #'as yet unclassified' }
MFClassB >> method2 [
	"This instance variable is there to trigger the 'Temporary variable not read or written' critics rule for the test SmallintManifestCheckerTest>>testCriticsOf rule"
	|foo|
	"Having a line of code (for example the following one) here is necessary not to fail the tests SmallintManifestCheckerTest>>testIsToDo and SmallintManifestCheckerTest>>testToDoOf"
	1.
]

{ #category : #'as yet unclassified' }
MFClassB >> method3 [
	"This instance variable is there to trigger the 'Temporary variable not read or written' critics rule for the test SmallintManifestCheckerTest>>testCriticsOf rule"
	|foo|
]
