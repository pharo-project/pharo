versions
version02: spec
	<version: '0.2' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec
			package: 'Ston-Core' with: 'Ston-Core-SvenVanCaekenberghe.36';
			package: 'Ston-Tests' with: 'Ston-Tests-SvenVanCaekenberghe.34'. ].
