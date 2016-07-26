protected
privateMappers

	^ OrderedCollection new
		addAll: self globalMappers;
		add: self generalMapper;
		reduceRight: [:left :right | left next: right ];
		yourself