versions
version15: spec
	<version: '0.15' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec 
			blessing: #'release';
			package: 'STON-Core' with: 'STON-Core-SvenVanCaekenberghe.63';
			package: 'STON-Tests' with: 'STON-Tests-SvenVanCaekenberghe.58' ]