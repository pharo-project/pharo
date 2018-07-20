Class {
	#name : #NewMethodFinderBottomClass,
	#superclass : #NewMethodFinderTopClass,
	#instVars : [
		'instanceVariable'
	],
	#category : #'NewMethodFinder-Tests'
}

{ #category : #'method finder' }
NewMethodFinderBottomClass class >> approvedSelectors [

	^#(topDisapprovedButBottomApprovedMethod onlyBottomDefinedApprovedMethod modifyMethod: oneArgumentMethod: otherOneArgumentMethod: InApprovedAndInForbiddenSetMethod)
]

{ #category : #'method finder' }
NewMethodFinderBottomClass class >> forbiddenSelectors [

	^#(topApprovedButBottomDisapprovedMethod InApprovedAndInForbiddenSetMethod)
]

{ #category : #comparing }
NewMethodFinderBottomClass >> = other [

	^ instanceVariable = other instanceVariable.
]

{ #category : #'as yet unclassified' }
NewMethodFinderBottomClass >> InApprovedAndInForbiddenSetMethod [

	 ^ self
]

{ #category : #accessing }
NewMethodFinderBottomClass >> instanceVariable [

	^ instanceVariable.
]

{ #category : #'as yet unclassified' }
NewMethodFinderBottomClass >> modifyMethod: x [

	instanceVariable := x.
	^ x.
]

{ #category : #'as yet unclassified' }
NewMethodFinderBottomClass >> multipleArgumentsMethod: x and: y [

	^ x
]

{ #category : #'as yet unclassified' }
NewMethodFinderBottomClass >> oneArgumentMethod: x [

	^ x + 0
]

{ #category : #'as yet unclassified' }
NewMethodFinderBottomClass >> onlyBottomDefinedApprovedMethod [

	^self
]

{ #category : #'as yet unclassified' }
NewMethodFinderBottomClass >> onlyBottomDefinedDisapprovedMethod [

	^self
]

{ #category : #'as yet unclassified' }
NewMethodFinderBottomClass >> otherOneArgumentMethod: s [

	^ s, ''
]

{ #category : #'as yet unclassified' }
NewMethodFinderBottomClass >> topApprovedButBottomDisapprovedMethod [

	^self
]

{ #category : #'as yet unclassified' }
NewMethodFinderBottomClass >> topDisapprovedButBottomApprovedMethod [

	^self
]
