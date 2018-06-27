Class {
	#name : #NewMethodFinderTopClass,
	#superclass : #Object,
	#category : #NewMethodFinder
}

{ #category : #'method finder' }
NewMethodFinderTopClass class >> approvedMethods [

	^#(topApprovedButBottomDisapprovedMethod onlyTopDefinedApprovedMethod)
]

{ #category : #'as yet unclassified' }
NewMethodFinderTopClass >> onlyTopDefinedApprovedMethod [
	"comment stating purpose of message"
	^ self
]

{ #category : #'as yet unclassified' }
NewMethodFinderTopClass >> onlyTopDefinedDisaprovedMethod [

	^self
]

{ #category : #'as yet unclassified' }
NewMethodFinderTopClass >> topApprovedButBottomDisapprovedMethod [

	^self
]

{ #category : #'as yet unclassified' }
NewMethodFinderTopClass >> topDisapprovedButBottomApprovedMethod [

	^self
]
