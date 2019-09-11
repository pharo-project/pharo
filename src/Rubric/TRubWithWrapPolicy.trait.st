"
I am a Trait that can be used by subclasses of RubScrolledTextExtra wanting a WrapPolicy.
"
Trait {
	#name : #TRubWithWrapPolicy,
	#category : #'Rubric-Traits'
}

{ #category : #'managing wrapping policy' }
TRubWithWrapPolicy >> changeWrappingPolicy [
	self paragraphProvider wrapped
		ifTrue: [ self paragraphProvider beNotWrapped ]
		ifFalse: [ self paragraphProvider beWrapped ].
	self updateContents.
	self paragraphProvider changed
]

{ #category : #accessing }
TRubWithWrapPolicy >> fontToUse [
	^ RubAbstractTextArea lineNumbersFont 
]

{ #category : #'event handling' }
TRubWithWrapPolicy >> updateContents [
	self explicitRequirement
]

{ #category : #'managing wrapping policy' }
TRubWithWrapPolicy >> wrappingPolicyLabel [
	^ self paragraphProvider wrapped
		ifTrue: [ 'W' ]
		ifFalse: [ 'NW' ]
]

{ #category : #'managing wrapping policy' }
TRubWithWrapPolicy >> wrappingPolicyLabelMorph [
	^ StringMorph contents: self wrappingPolicyLabel font: self fontToUse
]
