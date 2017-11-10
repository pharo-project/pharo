"
This help type extends the system with a wiki like help descriptions
"
Class {
	#name : #WikiStyleHelp,
	#superclass : #Object,
	#category : #HelpSystem-Core-Utilities
}

{ #category : #conversion }
WikiStyleHelp class >> asHelpTopic [ 
	<systemHelp>
	
	^WikiStyleHelpBuilder new build topicToBuild
]
