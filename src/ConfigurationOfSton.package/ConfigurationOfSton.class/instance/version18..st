versions
version18: spec
	<version: '0.18' imports: #('0.3-baseline')>

	spec for: #'common' do: [
		spec 
			blessing: #'release';
			package: 'STON-Core' with: 'STON-Core-SvenVanCaekenberghe.72';
			package: 'STON-Tests' with: 'STON-Tests-SvenVanCaekenberghe.64' ]