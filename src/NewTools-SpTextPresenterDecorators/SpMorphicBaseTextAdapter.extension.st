Extension { #name : #SpMorphicBaseTextAdapter }

{ #category : #'*NewTools-SpTextPresenterDecorators' }
SpMorphicBaseTextAdapter >> addTextSegmentDecoration: aDecorationSegment [
	self widget
		ifNotNil:
			[ :w | w addSegment: (RubUnderlinedSegmentMorph on: aDecorationSegment) ]
]

{ #category : #'*NewTools-SpTextPresenterDecorators' }
SpMorphicBaseTextAdapter >> removeAllTextSegmentDecorations [
	self widget
		ifNotNil: [ :w | w segments do: [ :s | w textArea removeSegment: s ] ]
]
