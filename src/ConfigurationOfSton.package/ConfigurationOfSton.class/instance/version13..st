versions
version13: spec
	<version: '0.13' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec 
			blessing: #'release';
			package: 'STON-Core' with: 'STON-Core-SvenVanCaekenberghe.57';
			package: 'STON-Tests' with: 'STON-Tests-SvenVanCaekenberghe.52' ]