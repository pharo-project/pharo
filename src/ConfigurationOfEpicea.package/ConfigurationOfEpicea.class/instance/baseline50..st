baselines
baseline50: spec
	<version: '5.0-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: self mainRepositoryUrlString.

		self baseline13STON: spec.

		spec
			postLoadDoIt: #version50PostLoad.

		spec 
			package: 'Ombu' with: [
				spec requires: #(STON) ]; "Ston was not integrated in Pharo 40"
			package: 'Epicea' with: [
				spec requires: #(Ombu) ];
			package: 'EpiceaKomitter' with: [
				spec requires: #(Epicea) ];
			package: 'EpiceaTriggering' with: [
				spec requires: #(Epicea) ];
			package: 'EpiceaInPharo40' with: [
				spec requires: #(Epicea) ].

		spec group: 'default' with: #(EpiceaInPharo40).
		spec group: 'triggering' with: #(EpiceaTriggering EpiceaInPharo40) ].

	spec for: #'pharo5.x' do: [
		spec package: 'Ombu' overrides: [
			spec requires: #() ]. "Ston is integrated in Pharo 50"
		
		spec group: 'default' overrides: #(Epicea).
		spec group: 'triggering' overrides: #(EpiceaTriggering Epicea) ].
