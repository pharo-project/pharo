versions
version06: spec
	<version: '0.6' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec 
			blessing: #'release';
			package: 'STON-Core' with: 'STON-Core-SvenVanCaekenberghe.45';
			package: 'STON-Tests' with: 'STON-Tests-SvenVanCaekenberghe.42' ]