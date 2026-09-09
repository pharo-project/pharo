"
i represent close segment (when user issuing #close command to path builder)
"
Class {
	#name : 'AthensCloseSegment',
	#superclass : 'AthensLineSegment',
	#category : 'Athens-Core-Paths',
	#package : 'Athens-Core',
	#tag : 'Paths'
}

{ #category : 'visitor' }
AthensCloseSegment >> accept: aVisitor [

	<ignoreNotImplementedSelectors: #(closeSegment:)>
	^ aVisitor closeSegment: self
]

{ #category : 'visitor' }
AthensCloseSegment >> sendCommandTo: aBuilder [
	^ aBuilder close
]

{ #category : 'visitor' }
AthensCloseSegment >> visitWith: visitor [
	visitor visitCloseSegment:  self
]
