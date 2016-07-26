baselines
baseline10: spec
	<version: '1.0-baseline'>
	spec
		for: #common
		do: [ 
			spec blessing: #baseline.
			spec
				repository: self mainRepositoryUrlString;
				package: 'Ombu' with: [ spec requires: 'Phexample' ];
				package: 'Epicea' with: [ spec requires: #(Ombu) ].
				
			self baseline10Phexample: spec.

			spec group: 'default' with: #(Epicea) ]