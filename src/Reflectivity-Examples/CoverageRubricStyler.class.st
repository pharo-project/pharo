"
A styler that highlight with green all executed statement.
For simplicity we extend from MetaLinkIconStyler
"
Class {
	#name : #CoverageRubricStyler,
	#superclass : #MetalinkIconStyler,
	#category : #Reflectivity-Examples
}

{ #category : #styling }
CoverageRubricStyler >> addIconStyle: aNode from: start to: stop [
	|rejectClass|
	
	rejectClass := { RBMethodNode . RBSequenceNode }.
	(rejectClass includes: aNode class) ifTrue: [ ^self ]. "do nothing" 
	aNode hasBeenExecuted ifTrue: [  super addIconStyle: aNode from: start to: stop]
]
