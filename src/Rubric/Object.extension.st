Extension { #name : #Object }

{ #category : #'*Rubric' }
Object >> annotateRubricText: anAnnotatableTextModel [
	| conf |

	conf := RubConfigurationChange new.
	
	conf configurationBlock: [ :text |
		self qaTextSegmentsDo: [ :segment | 
			text addSegment: segment ] ].

	anAnnotatableTextModel announce: conf
]

{ #category : #'*Rubric' }
Object >> unplug [

]
