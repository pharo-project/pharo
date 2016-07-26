*qualityAssistant
annotateRubricText: anAnnotatableTextModel
	| conf |

	conf := RubConfigurationChange new.
	
	conf configurationBlock: [ :text |
		self externalProperties do: [ :eprop |
			|r interval|
			interval := eprop sourceAnchor providesInterval
				ifTrue:  [ eprop sourceAnchor interval ]
				ifFalse: [ 0 to: -1 ]. 
		r := QARubTextSegmentMorph from: interval first to: interval last + 1.
		text addSegment: r.
		
		r label: eprop title.
		r icon: eprop icon.
		r color: eprop color.
		r critique: eprop.
		r entity: self]].

	anAnnotatableTextModel announce: conf