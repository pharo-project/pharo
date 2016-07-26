baselines
baseline71p5: spec
	<version: '7.1.p5-baseline'>
	"Specialized baseline for Pharo 5"

	spec for: #'pharo5.x' do: [
		spec blessing: #'baseline'.
		spec repository: self mainRepositoryUrlString.

		self baseline64SMark: spec.

		spec postLoadDoIt: #version60PostLoad.

		spec 
			package: 'Ombu';
			package: 'Epicea' with: [
				spec requires: #(Ombu) ];
			package: 'EpiceaBrowsers' with: [
				spec requires: #(Epicea) ];
			package: 'EpiceaTests' with: [
				spec requires: #(Epicea) ];
			package: 'EpiceaBrowsersTests' with: [
				spec requires: #(EpiceaBrowsers EpiceaTests) ];
			package: 'EpiceaBenchmarks' with: [ 
				spec requires: #(EpiceaBrowsersTests SMark) ];
			package: 'EpiceaKomitter' with: [
				spec requires: #(EpiceaBrowsersTests) ];
			package: 'EpiceaSUnit' with: [
				spec requires: #(EpiceaBrowsersTests) ];
			package: 'Hiedra';
			package: 'EpiceaHiedra' with: [
				spec requires: #(EpiceaBrowsers Hiedra) ].

		spec group: 'default' overrides: #(EpiceaBrowsers EpiceaBrowsersTests).
		spec group: 'core' overrides: #(EpiceaBrowsers).
		spec group: 'benchmarks' with: #(EpiceaBrowsers EpiceaBrowsersTests EpiceaBenchmarks).
		spec group: 'komitter' with: #(EpiceaBrowsers EpiceaBrowsersTests EpiceaKomitter).
		spec group: 'hiedra' with: #(EpiceaBrowsers EpiceaBrowsersTests EpiceaHiedra).
		spec group: 'sunit' with: #(EpiceaBrowsers EpiceaBrowsersTests EpiceaSUnit).
	].
