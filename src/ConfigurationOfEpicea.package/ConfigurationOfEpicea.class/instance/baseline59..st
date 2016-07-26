baselines
baseline59: spec
	<version: '5.9-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: self mainRepositoryUrlString.

		self baseline13STON: spec.

		spec postLoadDoIt: #version59PostLoad.

		spec 
			package: 'Ombu' with: [
				spec requires: #(STON) ]; "Ston was not integrated in Pharo 40"
			package: 'Epicea' with: [
				spec requires: #(Ombu) ];
			package: 'EpiceaKomitter' with: [
				spec requires: #(Epicea) ];
			package: 'EpiceaInPharo40' with: [
				spec requires: #(Epicea) ];
			package: 'EpiceaInPharo50' with: [
				spec requires: #(Epicea) ].		

		spec group: 'default' with: #(EpiceaInPharo40).
		spec group: 'triggering' with: #(EpiceaInPharo40). "Kept only for backward-compatibility"
		spec group: 'komitter' with: #(EpiceaInPharo40 EpiceaKomitter).
	].

	spec for: #'pharo5.x' do: [
		spec package: 'Ombu' overrides: [
			spec requires: #() ]. "Ston is integrated in Pharo 50"

		spec group: 'default' overrides: #(EpiceaInPharo50).
		spec group: 'triggering' overrides: #(EpiceaInPharo50). "Kept only for backward-compatibility"
		spec group: 'komitter' with: #(EpiceaInPharo50 EpiceaKomitter).
	].
