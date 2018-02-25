"
A help topic on how to contribute
"
Class {
	#name : #HowToContributeHelp,
	#superclass : #CustomHelp,
	#category : #HowToContributeHelp
}

{ #category : #accessing }
HowToContributeHelp class >> bookName [
	^'Contribute to base code'
]

{ #category : #pages }
HowToContributeHelp class >> introduction [
	"This method was automatically generated."
	<script: 'self edit: #introduction'>
	^HelpTopic
		title: 'Introduction'
		contents: 
'This help summarizes the process to contribute to the Pharo base code. Proposing a bug fix or enhancement is easy and relatively well documented. 
The following website contains a clear description of the change:
	http://pharo.org/contribute-propose-fix 
 !' readStream nextChunkText
]

{ #category : #accessing }
HowToContributeHelp class >> pages [
	^#(introduction)
]
