versions
version01: spec
	<version: '0.1' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: ''.
		spec author: 'SeanDeNigris'.
		spec timestamp: '7/5/2012 19:06'.
		spec
			package: 'Ston-Core' with: 'Ston-Core-SvenVanCaekenberghe.35';
			package: 'Ston-Tests' with: 'Ston-Tests-SvenVanCaekenberghe.33'. ].
