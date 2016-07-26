baselines
baseline43: spec
	<version: '4.3-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: self mainRepositoryUrlString.

		self baseline13STON: spec.

		spec
			preLoadDoIt: #version23PreLoad;
			postLoadDoIt: #version41PostLoad.
				
		spec 
			package: 'Ombu' with: [
				spec requires: #(STON) ];
			package: 'Epicea' with: [
				spec requires: #(Ombu) ];
			package: 'EpiceaKomitter' with: [
				spec requires: #(Epicea) ].

		spec group: 'default' with: #(EpiceaKomitter). ].
	
	spec for: #'pharo3.0.x' do: [		
		spec 
			package: 'EpiceaInPharo30';
			package: 'Epicea' with: [
				spec requires: #(Ombu EpiceaInPharo30) ].
	].			
			
