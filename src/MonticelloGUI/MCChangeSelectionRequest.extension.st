Extension { #name : #MCChangeSelectionRequest }

{ #category : #'*MonticelloGUI' }
MCChangeSelectionRequest >> defaultAction [
	^ (MCChangeSelector new patch: patch; label: label) showModally
]
