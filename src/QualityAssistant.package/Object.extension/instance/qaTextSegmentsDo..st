*qualityAssistant
qaTextSegmentsDo: aBlock
	
	self externalProperties do: [ :eprop |
		| segment interval |
		interval := eprop sourceAnchor providesInterval
			ifTrue:  [ eprop sourceAnchor interval ]
			ifFalse: [ 0 to: -1 ]. 
		
		segment := QARubTextSegmentMorph
			from: interval first
			to: interval last + 1.

		segment
			label: eprop title;
			icon: eprop icon;
			color: eprop color;
			critique: eprop;
			entity: self.
		
		aBlock value: segment ].