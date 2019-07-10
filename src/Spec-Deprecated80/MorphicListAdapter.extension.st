Extension { #name : #MorphicListAdapter }

{ #category : #'*Spec-Deprecated80' }
MorphicListAdapter >> getSelectionStateFor: anIndex [
	self deprecated: 'This method from the old API will be removed.'.
	^ self model getSelectionStateFor: anIndex
]
