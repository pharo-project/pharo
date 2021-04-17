Class {
	#name : #MicrodownisedHelp,
	#superclass : #Object,
	#category : #'HelpSystem-Core-Utilities'
}

{ #category : #conversion }
MicrodownisedHelp class >> asHelpTopic [ 
	<newSystemHelp>
	
	^ MicrodownStyleHelpBuilder new build topicToBuild
]
