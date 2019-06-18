"
See comment of my collate:with: method
"
Class {
	#name : #ClySortMethodByPackageFunction,
	#superclass : #ClySortFunction,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #evaluating }
ClySortMethodByPackageFunction >> collate: aMethod1 with: aMethod2 [
	"Returns:
		-1 if aMethod1 is 'lower than' aMethod2
		0 if aMethod1 is 'equal to' aMethod2
		1 if aMethod1 is 'greater than' aMethod2
		
	aMethod1 is considered 'lower than' aMethod2 if:
		the name of aMethod1's package is lower than aMethod2's 
		or if the package names are the same and the name of aMethod1's class is lower than aMethod2's
		or if the package names and the class names are the same, and aMethod1's name is lower than aMethod2's name"
	| comparisonResult |
	comparisonResult := (aMethod1 package name threeWayCompareTo: aMethod2 package name).
	(comparisonResult = 0) ifFalse: [ ^ comparisonResult ].
	
	comparisonResult := (aMethod1 origin name threeWayCompareTo: aMethod2 origin name).
	(comparisonResult = 0) ifFalse: [ ^ comparisonResult ].

	^aMethod1 selector threeWayCompareTo: aMethod2 selector
]
