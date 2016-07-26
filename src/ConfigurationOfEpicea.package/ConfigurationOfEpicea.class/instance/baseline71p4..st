baselines
baseline71p4: spec
	<version: '7.1.p4-baseline'>
	"Specialized baseline for Pharo 4"

	spec for: #'pharo4.x' do: [
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
			package: 'EpiceaBrowsers' with: [
				spec requires: #(Epicea) ];
			package: 'EpiceaTests' with: [
				spec requires: #(Epicea) ];
			package: 'EpiceaBrowsersTests' with: [
				spec requires: #(EpiceaBrowsers EpiceaTests) ];
			package: 'EpiceaInPharo40' with: [
				spec requires: #(EpiceaBrowsers) ].

		spec group: 'default' with: #(EpiceaInPharo40 EpiceaBrowsersTests).
	].