protected
mappers

	^ OrderedCollection new
		addAll: self pluggableSubstitutionMappers;
		addAll: self globalMappers;
		add: self generalMapper;
		reduceRight: [:left :right | left next: right ];
		yourself