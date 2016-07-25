baselines
baseline14: spec
	<version: '1.4-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: self mainRepositoryUrlString.
		self baseline10Phexample: spec.
		self baseline13STON: spec.
		spec 
			package: 'Ombu' with: [
				spec requires: #('STON'). ];
			package: 'Epicea' with: [
				spec requires: #(Ombu). ].
		spec group: 'default' with: #(Epicea). ].
