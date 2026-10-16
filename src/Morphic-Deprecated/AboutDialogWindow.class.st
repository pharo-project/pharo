"
Default superclass for application about dialogs.
"
Class {
	#name : 'AboutDialogWindow',
	#superclass : 'LongMessageDialogWindow',
	#category : 'Morphic-Deprecated',
	#package : 'Morphic-Deprecated'
}

{ #category : 'instance creation' }
AboutDialogWindow class >> open [
	<script>

	^(self new)
		open;
		yourself
]

{ #category : 'defaults' }
AboutDialogWindow >> defaultLabel [
	"Answer the default label for the receiver."

	^'About'
]
