baselines
baseline33: spec
	<version: '3.3-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: self mainRepositoryUrlString.

		self baseline13STON: spec.

		spec
			preLoadDoIt: #version23PreLoad;
			postLoadDoIt: #version33PostLoad.
				
		spec 
			package: 'Ombu' with: [
				spec requires: #(STON). ];
			package: 'Epicea' with: [
				spec requires: #(Ombu). ];
			package: 'EpiceaExamples' with: [
				spec requires: #(Epicea). ].

		spec group: 'default' with: #(Epicea). ].