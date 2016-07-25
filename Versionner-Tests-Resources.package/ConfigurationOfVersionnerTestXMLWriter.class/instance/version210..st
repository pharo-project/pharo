versions
version210: spec
	<version: '2.1.0' imports: #('2.1.0-baseline')>
	spec
		for: #common
			do: [ spec
					blessing: #release;
					package: 'XML-Writer-Core' with: 'XML-Writer-Core-StephaneDucasse.1' ;
					package: 'XML-Writer-Tests' with: 'XML-Writer-Tests-StephaneDucasse.1' ];
		
		" I do not understand the following line because it means that we would load an older version than the one currently in Pharo. Clearly the best way to introduce bugs and losing hours trying to understand why.
		for: #squeakCommon do: [ spec package: 'Collections-Support' with: 'Collections-Support-StephaneDucasse.35' ];"
		for: #gemstone
			do: [ 
					spec
						package: 'Collections-Support' with: 'Collections-Support.g-OttoBehrens.45';
						package: 'Collections-Tests-Support' with: 'Collections-Tests-Support.g-OttoBehrens.3' ]