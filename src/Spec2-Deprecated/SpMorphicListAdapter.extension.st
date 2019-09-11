Extension { #name : #SpMorphicListAdapter }

{ #category : #'*Spec2-Deprecated' }
SpMorphicListAdapter >> getSelectionStateFor: anIndex [
	self deprecated: 'This method from the old API will be removed.'.
	^ self model getSelectionStateFor: anIndex
]
