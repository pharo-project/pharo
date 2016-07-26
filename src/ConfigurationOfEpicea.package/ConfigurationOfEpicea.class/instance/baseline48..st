baselines
baseline48: spec
	<version: '4.8-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: self mainRepositoryUrlString.

		self baseline13STON: spec.

		spec 
			package: 'Ombu' with: [
				spec requires: #(STON) ];
			package: 'Epicea' with: [
				spec requires: #(Ombu) ];
			package: 'EpiceaKomitter' with: [
				spec requires: #(Epicea) ];
			package: 'EpiceaTriggering' with: [
				spec requires: #(Epicea) ].


		spec group: 'default' with: #(Epicea).
		spec group: 'triggering' with: #(EpiceaTriggering) ].
	
	spec for: #'pharo3.0.x' do: [		
		spec 
			package: 'EpiceaInPharo30';
			package: 'Epicea' with: [
				spec requires: #(Ombu EpiceaInPharo30) ].
	].			
			
