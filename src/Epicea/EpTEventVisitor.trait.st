"
I am a visitor for EpEvent subinstances.
"
Trait {
	#name : #EpTEventVisitor,
	#traits : 'EpTCodeChangeVisitor',
	#classTraits : 'EpTCodeChangeVisitor classTrait',
	#category : #Epicea-Visitors
}

{ #category : #visitor }
EpTEventVisitor >> visitBehaviorCopy: anEvent [
	^ self visitEvent: anEvent
]

{ #category : #visitor }
EpTEventVisitor >> visitCodeChange: aChange [
	"Default behavior for any change."
		
	^ self visitEvent: aChange
]

{ #category : #visitor }
EpTEventVisitor >> visitEvent: aChange [
	^ self explicitRequirement
]

{ #category : #visitor }
EpTEventVisitor >> visitExpressionEvaluation: anExpressionEvaluation [
	^ self visitEvent: anExpressionEvaluation
]

{ #category : #visitor }
EpTEventVisitor >> visitLogCommentModification: anEvent [ 
	^ self visitEvent: anEvent
]

{ #category : #visitor }
EpTEventVisitor >> visitMonticelloVersionSave: anEvent [
	^ self visitEvent: anEvent
]

{ #category : #visitor }
EpTEventVisitor >> visitMonticelloVersionsLoad: anEvent [
	^ self visitEvent: anEvent
]

{ #category : #visitor }
EpTEventVisitor >> visitRedo: aRedoEvent [
	^ self visitEvent: aRedoEvent
]

{ #category : #visitor }
EpTEventVisitor >> visitRefactoring: anEvent [ 
	^ self visitEvent: anEvent
]

{ #category : #visitor }
EpTEventVisitor >> visitSessionEnd: anEvent [
	^ self visitEvent: anEvent
]

{ #category : #visitor }
EpTEventVisitor >> visitSessionSnapshot: anEvent [
	^ self visitEvent: anEvent
]

{ #category : #visitor }
EpTEventVisitor >> visitSessionStart: anEvent [ 
	^ self visitEvent: anEvent
]

{ #category : #visitor }
EpTEventVisitor >> visitUndo: anUndoEvent [
	^ self visitEvent: anUndoEvent
]
