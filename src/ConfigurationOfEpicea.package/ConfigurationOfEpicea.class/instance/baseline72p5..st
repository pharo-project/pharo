baselines
baseline72p5: spec
	<version: '7.2.p5-baseline'>
	"Specialized baseline for Pharo 5"

	spec for: #common do: [
		spec blessing: #'baseline'.
		spec repository: self mainRepositoryUrlString.

		self baseline64SMark: spec.

		spec postLoadDoIt: #version60PostLoad.

		spec 
			package: 'Ombu';
			package: 'OmbuTests' with: [
				spec requires: #(Ombu) ];
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

		spec group: 'default' overrides: #(EpiceaBrowsers EpiceaBrowsersTests OmbuTests).
		spec group: 'core' overrides: #(EpiceaBrowsers).
		spec group: 'benchmarks' with: #(EpiceaBrowsers EpiceaBrowsersTests OmbuTests EpiceaBenchmarks).
		spec group: 'komitter' with: #(EpiceaBrowsers EpiceaBrowsersTests OmbuTests EpiceaKomitter).
		spec group: 'hiedra' with: #(EpiceaBrowsers EpiceaBrowsersTests OmbuTests EpiceaHiedra).
		spec group: 'sunit' with: #(EpiceaBrowsers EpiceaBrowsersTests OmbuTests EpiceaSUnit).
	].
