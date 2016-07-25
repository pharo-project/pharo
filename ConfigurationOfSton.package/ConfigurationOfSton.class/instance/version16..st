versions
version16: spec
	<version: '0.16' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec 
			blessing: #'release';
			package: 'STON-Core' with: 'STON-Core-SvenVanCaekenberghe.66';
			package: 'STON-Tests' with: 'STON-Tests-SvenVanCaekenberghe.59' ]