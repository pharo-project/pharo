"
I'm a specialized controller, that works with Workspaces.
"
Class {
	#name : #NECWorkspaceController,
	#superclass : #NECController,
	#category : #NECompletion-View
}

{ #category : #accessing }
NECWorkspaceController >> additionals [
	^ self workspace completionAdditionals
]

{ #category : #accessing }
NECWorkspaceController >> workspace [
	^model
]
