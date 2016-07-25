baselines
baseline46: spec
	"Preparing for integration"
	<version: '4.6-baseline'>

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
				spec requires: #(Epicea) ].

		spec group: 'default' with: #(Epicea). ].
	
	spec for: #'pharo3.0.x' do: [		
		spec 
			package: 'EpiceaInPharo30';
			package: 'Epicea' with: [
				spec requires: #(Ombu EpiceaInPharo30) ].
	].			
			
