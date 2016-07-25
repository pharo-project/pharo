versions
version12: spec
	<version: '1.2' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: '- Move Seaside support into ConfigurationOfSeaside30
- update to latest set of packages.'.
		spec author: 'dkh'.
		spec timestamp: '9/1/2011 16:33'.
		spec 
			package: 'Zinc-HTTP' with: 'Zinc-HTTP-SvenVanCaekenberghe.188';
			package: 'Zinc-Tests' with: 'Zinc-Tests-SvenVanCaekenberghe.98';
			package: 'Zinc-AWS' with: 'Zinc-AWS-SvenVanCaekenberghe.7';
			package: 'Zinc-Zodiac' with: 'Zinc-Zodiac-SvenVanCaekenberghe.4';
			package: 'Zinc-Patch-HTTPSocket' with: 'Zinc-Patch-HTTPSocket-StephaneDucasse.2';
			package: 'Zodiac' with: 'Zodiac-Core-SvenVanCaekenberghe.8'. ].

	spec for: #'gemstone' do: [
		spec blessing: #'release'.
		spec 
			package: 'Zinc-GemstonePreliminary' with: 'Zinc-GemstonePreliminary-PaulDeBruicker.2';
			package: 'Zinc-Gemstone' with: 'Zinc-Gemstone-PaulDeBruicker.14';
			package: 'SocketStream' with: 'SocketStream-PaulDeBruicker.12'. ].
