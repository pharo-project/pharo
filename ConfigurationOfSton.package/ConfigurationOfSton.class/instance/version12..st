versions
version12: spec
	<version: '0.12' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec 
			blessing: #'release';
			package: 'STON-Core' with: 'STON-Core-SvenVanCaekenberghe.55';
			package: 'STON-Tests' with: 'STON-Tests-SvenVanCaekenberghe.50' ]