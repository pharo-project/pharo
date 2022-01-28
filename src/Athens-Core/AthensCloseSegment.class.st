"
i represent close segment (when user issuing #close command to path builder)
"
Class {
	#name : #AthensCloseSegment,
	#superclass : #AthensLineSegment,
	#category : #'Athens-Core-Paths'
}

{ #category : #visitor }
AthensCloseSegment >> accept: aVisitor [
	^ aVisitor closeSegment: self
]

{ #category : #visitor }
AthensCloseSegment >> sendCommandTo: aBuilder [
	^ aBuilder close
]
