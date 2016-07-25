versions
version09: spec
	<version: '0.9' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec 
			blessing: #'release';
			package: 'STON-Core' with: 'STON-Core-SvenVanCaekenberghe.49';
			package: 'STON-Tests' with: 'STON-Tests-SvenVanCaekenberghe.44' ]