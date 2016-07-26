baselines
baseline64: spec
	<version: '6.4-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: self mainRepositoryUrlString.

		self baseline13STON: spec.
		self baseline64SMark: spec.

		spec postLoadDoIt: #version60PostLoad.

		spec 
			package: 'Ombu' with: [
				spec requires: #(STON) ]; "Ston was not integrated in Pharo 40"
			package: 'Hiedra';
			package: 'EpiceaHiedra' with: [
				spec requires: #(Epicea Hiedra) ];
			package: 'Epicea' with: [
				spec requires: #(Ombu) ];
			package: 'EpiceaBenchmarks' with: [ 
				spec requires: #(Epicea SMark) ];
			package: 'EpiceaKomitter' with: [
				spec requires: #(Epicea) ];
			package: 'EpiceaInPharo40' with: [
				spec requires: #(Epicea) ];
			package: 'EpiceaInPharo50' with: [
				spec requires: #(Epicea) ].		

		spec group: 'default' with: #(EpiceaInPharo40).
		spec group: 'komitter' with: #(EpiceaInPharo40 EpiceaKomitter).
	].

	spec for: #'pharo5.x' do: [
		spec package: 'Ombu' overrides: [
			spec requires: #() ]. "Ston is integrated in Pharo 50"

		spec group: 'default' overrides: #(EpiceaInPharo50).
		spec group: 'benchmarks' with: #(EpiceaInPharo50 EpiceaBenchmarks).
		spec group: 'komitter' with: #(EpiceaInPharo50 EpiceaKomitter).
		spec group: 'hiedra' with: #(EpiceaInPharo50 EpiceaHiedra).
	].
