baselines
baseline66: spec
	<version: '6.6-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: self mainRepositoryUrlString.

		self baseline13STON: spec.
		self baseline64SMark: spec.

		spec postLoadDoIt: #version60PostLoad.

		spec 
			package: 'Ombu' with: [
				spec requires: #(STON) ]; "Ston was not integrated in Pharo 40"
			package: 'Epicea' with: [
				spec requires: #(Ombu) ];
			package: 'EpiceaTests' with: [
				spec requires: #(Epicea) ];
			package: 'EpiceaBenchmarks' with: [ 
				spec requires: #(EpiceaTests SMark) ];
			package: 'EpiceaKomitter' with: [
				spec requires: #(EpiceaTests) ];
			package: 'EpiceaSUnit' with: [
				spec requires: #(EpiceaTests) ];
			package: 'EpiceaInPharo40' with: [
				spec requires: #(Epicea) ];
			package: 'EpiceaInPharo50' with: [
				spec requires: #(Epicea) ];
			package: 'Hiedra';
			package: 'EpiceaHiedra' with: [
				spec requires: #(Epicea Hiedra) ].

		spec group: 'default' with: #(EpiceaInPharo40 EpiceaTests).
		spec group: 'komitter' with: #(EpiceaInPharo40 EpiceaTests EpiceaKomitter).
	].

	spec for: #'pharo5.x' do: [
		spec package: 'Ombu' overrides: [
			spec requires: #() ]. "Ston is integrated in Pharo 50"

		spec group: 'default' overrides: #(EpiceaInPharo50 EpiceaTests).
		spec group: 'core' overrides: #(EpiceaInPharo50).
		spec group: 'benchmarks' with: #(EpiceaInPharo50 EpiceaTests EpiceaBenchmarks).
		spec group: 'komitter' with: #(EpiceaInPharo50 EpiceaTests EpiceaKomitter).
		spec group: 'hiedra' with: #(EpiceaInPharo50 EpiceaTests EpiceaHiedra).
		spec group: 'sunit' with: #(EpiceaInPharo50 EpiceaTests EpiceaSUnit).
	].
