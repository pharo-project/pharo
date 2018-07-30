Class {
	#name : #MethodFinderTopClass,
	#superclass : #Object,
	#category : #'Tools-Test-Finder-Support'
}

{ #category : #'method finder' }
MethodFinderTopClass class >> approvedSelectorsForMethodFinder [

	^#(topApprovedButBottomDisapprovedMethod onlyTopDefinedApprovedMethod)
]

{ #category : #'method finder' }
MethodFinderTopClass class >> forbiddenSelectorsForMethodFinder [
	^ #(topDisapprovedButBottomApprovedMethod)
]

{ #category : #'as yet unclassified' }
MethodFinderTopClass >> onlyTopDefinedApprovedMethod [
	"comment stating purpose of message"
	^ self
]

{ #category : #'as yet unclassified' }
MethodFinderTopClass >> onlyTopDefinedDisaprovedMethod [

	^self
]

{ #category : #tests }
MethodFinderTopClass >> test [
	^ self onlyTopDefinedDisaprovedMethod 
]

{ #category : #'as yet unclassified' }
MethodFinderTopClass >> topApprovedButBottomDisapprovedMethod [

	^self
]

{ #category : #'as yet unclassified' }
MethodFinderTopClass >> topDisapprovedButBottomApprovedMethod [

	^self
]
