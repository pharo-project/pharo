Class {
	#name : #NewMethodFinderDummy,
	#superclass : #Object,
	#category : #NewMethodFinder
}

{ #category : #'method finder' }
NewMethodFinderDummy class >> approvedMethods [

	^ #(foo x:y:z: arg:)
]

{ #category : #'as yet unclassified' }
NewMethodFinderDummy >> arg:x [

	^x
]

{ #category : #'scrollbar-brick-accessing' }
NewMethodFinderDummy >> bar [

	^ self
]

{ #category : #accessing }
NewMethodFinderDummy >> foo [

	^ self
]

{ #category : #accessing }
NewMethodFinderDummy >> foo2 [

	^ self
]

{ #category : #'as yet unclassified' }
NewMethodFinderDummy >> x:x y:y z:z [
	"comment stating purpose of message"

	^ x + y + z
]
