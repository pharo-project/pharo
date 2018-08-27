Class {
	#name : #MethodFinderBottomClass,
	#superclass : #MethodFinderTopClass,
	#instVars : [
		'instanceVariable'
	],
	#category : #'Tools-Test-Finder-Support'
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

{ #category : #'as yet unclassified' }
MethodFinderBottomClass >> InApprovedAndInForbiddenSetMethod [

	 ^ self
]

{ #category : #accessing }
MethodFinderBottomClass >> instanceVariable [

	^ instanceVariable.
]

{ #category : #'as yet unclassified' }
MethodFinderBottomClass >> modifyMethod: x [

	instanceVariable := x.
	^ x.
]

{ #category : #'as yet unclassified' }
MethodFinderBottomClass >> multipleArgumentsMethod: x and: y [

	^ x
]

{ #category : #'as yet unclassified' }
MethodFinderBottomClass >> oneArgumentMethod: x [

	^ x + 0
]

{ #category : #'as yet unclassified' }
MethodFinderBottomClass >> onlyBottomDefinedApprovedMethod [

	^self
]

{ #category : #'as yet unclassified' }
MethodFinderBottomClass >> onlyBottomDefinedDisapprovedMethod [

	^self
]

{ #category : #'as yet unclassified' }
MethodFinderBottomClass >> otherOneArgumentMethod: s [

	^ s, ''
]

{ #category : #'as yet unclassified' }
MethodFinderBottomClass >> topApprovedButBottomDisapprovedMethod [

	^self
]

{ #category : #'as yet unclassified' }
MethodFinderBottomClass >> topDisapprovedButBottomApprovedMethod [

	^self
]
