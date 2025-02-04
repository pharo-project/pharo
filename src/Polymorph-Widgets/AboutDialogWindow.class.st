"
Default superclass for application about dialogs.
"
Class {
	#name : 'AboutDialogWindow',
	#superclass : 'LongMessageDialogWindow',
	#category : 'Polymorph-Widgets-Windows',
	#package : 'Polymorph-Widgets',
	#tag : 'Windows'
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

	^'About' translated
]
