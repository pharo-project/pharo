baselines
baseline20: spec
	<version: '2.0-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: self mainRepositoryUrlString.
		self baseline13STON: spec.
	
		spec 
			package: 'Ombu' with: [
				spec requires: #(STON). ];
			package: 'Epicea' with: [
				spec requires: #(Ombu). ];
			package: 'EpiceaExamples' with: [
				spec requires: #(Epicea). ].
		spec group: 'default' with: #(Epicea). ].