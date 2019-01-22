"
I'm a dummy class just to have a package that is not empty.
"
Class {
	#name : #MFClassA,
	#superclass : #Object,
	#category : #'Manifest-Resources-Tests'
}

{ #category : #'as yet unclassified' }
MFClassA >> method [
	"This variable is there to trigger the 'Temporary variable not read or written' critics rule for the test SmallintManifestCheckerTest>>testCriticsOf rule"
	|foo|
]
