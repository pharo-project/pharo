"
I am a Trait that can be used by subclasses of RubScrolledTextExtra wanting to display or not line numbers.
"
Trait {
	#name : #TRubWithLineNumber,
	#category : #'Rubric-Traits'
}

{ #category : #'managing line numbers' }
TRubWithLineNumber >> changeLineNumbersDisplay [
	self paragraphProvider lineNumbersRuler 
		ifNil: [ self paragraphProvider withLineNumbers ]
		ifNotNil: [ self paragraphProvider withoutLineNumbers ].
	self paragraphProvider manageLayout.
	self updateContents
]

{ #category : #accessing }
TRubWithLineNumber >> fontToUse [
	^ RubAbstractTextArea lineNumbersFont 
]

{ #category : #'managing line numbers' }
TRubWithLineNumber >> lineNumbersDisplayLabel [
	^ self paragraphProvider lineNumbersRuler  
		ifNil: [ '+L' ]
		ifNotNil: [ 'L' ]
]

{ #category : #'managing line numbers' }
TRubWithLineNumber >> lineNumbersDisplayLabelMorph [
	^ StringMorph contents: self lineNumbersDisplayLabel font: self fontToUse
]

{ #category : #'event handling' }
TRubWithLineNumber >> updateContents [
	self explicitRequirement
]
