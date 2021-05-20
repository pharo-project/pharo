Extension { #name : #RubTextEditor }

{ #category : #'*Spec2-Adapters-Morphic' }
RubTextEditor >> lineAtCursorPosition [
	| string lastEnd index lastStart |
	
	index := self pointIndex.
	string := self text asString.
	string lineIndicesDo: [ :start :endWithoutDelimiters :end | 
		index <= end ifTrue: [ 
			^ string copyFrom: start to: endWithoutDelimiters ].
		lastStart := start.
		lastEnd := end ].
	
	"evaluate the case where the cursor is placed at the end of the text (there will not 
	 be delimiter, but there will be a line to answer anyway (maybe empty)"
	^ lastEnd + 1 <= index 
		ifTrue: [ string copyFrom: lastStart to: lastEnd ]
		ifFalse: [ '' ]
]
