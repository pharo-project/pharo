Class {
	#name : #NewMethodFinderTopClass,
	#superclass : #Object,
	#category : #'NewMethodFinder-Tests'
}

{ #category : #'method finder' }
NewMethodFinderTopClass class >> approvedSelectors [

	^#(topApprovedButBottomDisapprovedMethod onlyTopDefinedApprovedMethod)
]

{ #category : #'method finder' }
NewMethodFinderTopClass class >> forbiddenSelectors [
	^ #(topDisapprovedButBottomApprovedMethod)
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

{ #category : #tests }
NewMethodFinderTopClass >> test [
	^ self onlyTopDefinedDisaprovedMethod 
]

{ #category : #'as yet unclassified' }
NewMethodFinderTopClass >> topApprovedButBottomDisapprovedMethod [

	^self
]

{ #category : #'as yet unclassified' }
NewMethodFinderTopClass >> topDisapprovedButBottomApprovedMethod [

	^self
]
