"
Test sample for MethodFinderTest
"
Class {
	#name : #MethodFinderBottomClass,
	#superclass : #MethodFinderTopClass,
	#instVars : [
		'instanceVariable'
	],
	#category : #'Tool-Finder-Tests-Support'
}

{ #category : #'method finder' }
MethodFinderBottomClass class >> approvedSelectorsForMethodFinder [

	^#(topDisapprovedButBottomApprovedMethod onlyBottomDefinedApprovedMethod modifyMethod: oneArgumentMethod: otherOneArgumentMethod: InApprovedAndInForbiddenSetMethod)
]

{ #category : #'method finder' }
MethodFinderBottomClass class >> forbiddenSelectorsForMethodFinder [

	^#(topApprovedButBottomDisapprovedMethod InApprovedAndInForbiddenSetMethod)
]

{ #category : #comparing }
MethodFinderBottomClass >> = other [

	^ instanceVariable = other instanceVariable.
]

{ #category : #accessing }
MethodFinderBottomClass >> InApprovedAndInForbiddenSetMethod [

	 ^ self
]

{ #category : #accessing }
MethodFinderBottomClass >> instanceVariable [

	^ instanceVariable.
]

{ #category : #accessing }
MethodFinderBottomClass >> modifyMethod: x [

	instanceVariable := x.
	^ x.
]

{ #category : #accessing }
MethodFinderBottomClass >> multipleArgumentsMethod: x and: y [

	^ x
]

{ #category : #accessing }
MethodFinderBottomClass >> oneArgumentMethod: x [

	^ x + 0
]

{ #category : #accessing }
MethodFinderBottomClass >> onlyBottomDefinedApprovedMethod [

	^self
]

{ #category : #accessing }
MethodFinderBottomClass >> onlyBottomDefinedDisapprovedMethod [

	^self
]

{ #category : #accessing }
MethodFinderBottomClass >> otherOneArgumentMethod: s [

	^ s, ''
]

{ #category : #accessing }
MethodFinderBottomClass >> topApprovedButBottomDisapprovedMethod [

	^self
]

{ #category : #accessing }
MethodFinderBottomClass >> topDisapprovedButBottomApprovedMethod [

	^self
]
