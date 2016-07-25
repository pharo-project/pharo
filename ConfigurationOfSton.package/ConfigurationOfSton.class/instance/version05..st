versions
version05: spec
	<version: '0.5' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec 
			blessing: #'release';
			package: 'STON-Core' with: 'STON-Core-SvenVanCaekenberghe.43';
			package: 'STON-Tests' with: 'STON-Tests-SvenVanCaekenberghe.40' ]