"
Same as search string source anchor but uses specific heuristics for finding a better string based on the fact that we are looking in a class definition
"
Class {
	#name : #ReVarSearchSourceAnchor,
	#superclass : #ReSearchStringSourceAnchor,
	#category : #'Renraku-SourceAnchors'
}

{ #category : #accessing }
ReVarSearchSourceAnchor >> interval [
	^ super interval allButLast
]

{ #category : #private }
ReVarSearchSourceAnchor >> modify: aString for: aStringCollection [

	aStringCollection size = 1 ifTrue: [
		^ aString surroundedBySingleQuotes ].
	(aStringCollection first sameContents: aString) ifTrue: [
		^ '''', aString, String space ].
	(aStringCollection last sameContents: aString) ifTrue: [
		^ String space, aString, '''' ].
	^ aString surroundedBy: String space
]

{ #category : #accessing }
ReVarSearchSourceAnchor >> searchString [
	| origString |

	origString := super searchString.
	(sourceEntity instVarNames includes: origString) ifTrue: [
		^ self modify: origString for: sourceEntity instVarNames ].
	(sourceEntity classVarNames includes: origString) ifTrue: [
		^ self modify: origString for: sourceEntity classVarNames ].

	^ origString
]
