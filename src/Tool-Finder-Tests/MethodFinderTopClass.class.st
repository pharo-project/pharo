"
Test sample for MethodFinderTest
"
Class {
	#name : #MethodFinderTopClass,
	#superclass : #Object,
	#category : #'Tool-Finder-Tests-Support'
}

{ #category : #'method finder' }
MethodFinderTopClass class >> approvedSelectorsForMethodFinder [

	^#(topApprovedButBottomDisapprovedMethod onlyTopDefinedApprovedMethod)
]

{ #category : #'method finder' }
MethodFinderTopClass class >> forbiddenSelectorsForMethodFinder [
	^ #(topDisapprovedButBottomApprovedMethod)
]

{ #category : #accessing }
MethodFinderTopClass >> onlyTopDefinedApprovedMethod [
	"comment stating purpose of message"
	^ self
]

{ #category : #accessing }
MethodFinderTopClass >> onlyTopDefinedDisaprovedMethod [

	^self
]

{ #category : #tests }
MethodFinderTopClass >> test [
	^ self onlyTopDefinedDisaprovedMethod 
]

{ #category : #accessing }
MethodFinderTopClass >> topApprovedButBottomDisapprovedMethod [

	^self
]

{ #category : #accessing }
MethodFinderTopClass >> topDisapprovedButBottomApprovedMethod [

	^self
]
