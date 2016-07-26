versions
version03: spec
	<version: '0.3' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec
			package: 'STON-Core' with: 'STON-Core-SvenVanCaekenberghe.37';
			package: 'STON-Tests' with: 'STON-Tests-SvenVanCaekenberghe.29'. ]