versions
version04: spec
	<version: '0.4' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec
			package: 'STON-Core' with: 'STON-Core-SvenVanCaekenberghe.42';
			package: 'STON-Tests' with: 'STON-Tests-SvenVanCaekenberghe.39'. ]