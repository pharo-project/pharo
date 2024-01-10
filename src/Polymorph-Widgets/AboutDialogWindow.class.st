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

{ #category : 'instance creation' }
AboutDialogWindow class >> openForPharo [
	<script>
	| text dialog width |
	text := Smalltalk systemInformationString withCRs.
	width := 0.
	text
		linesDo: [ :l |
			width := width
				max: (self theme textFont widthOfStringOrText: l) ].
	dialog := self new entryText: text.
	dialog iconMorph formSet: (self iconFormSetNamed: #pharo).
	dialog title: 'About Pharo'.
	dialog open.
	dialog textMorph
		minWidth: 20;
		minHeight: 20.
	dialog position: 25 @ 25
]

{ #category : 'defaults' }
AboutDialogWindow >> defaultLabel [
	"Answer the default label for the receiver."

	^'About' translated
]
