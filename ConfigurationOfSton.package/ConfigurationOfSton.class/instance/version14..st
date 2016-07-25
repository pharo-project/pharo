versions
version14: spec
	<version: '0.14' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec 
			blessing: #'release';
			package: 'STON-Core' with: 'STON-Core-SvenVanCaekenberghe.58';
			package: 'STON-Tests' with: 'STON-Tests-SvenVanCaekenberghe.53' ]