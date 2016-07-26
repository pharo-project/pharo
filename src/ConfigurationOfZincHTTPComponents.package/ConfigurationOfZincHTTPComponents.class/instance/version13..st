versions
version13: spec
	<version: '1.3' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '-added Zinc-REST - update to latest set of packages.'.
		spec author: 'PaulDeBruicker'.
		spec timestamp: '10/26/2011 17:15'.
		spec 
			package: 'Zinc-HTTP' with: 'Zinc-HTTP-SvenVanCaekenberghe.214';
			package: 'Zinc-Tests' with: 'Zinc-Tests-SvenVanCaekenberghe.110';
			package: 'Zinc-AWS' with: 'Zinc-AWS-SvenVanCaekenberghe.9';
			package: 'Zinc-REST' with: 'Zinc-REST-SvenVanCaekenberghe.5';
			package: 'Zinc-Zodiac' with: 'Zinc-Zodiac-SvenVanCaekenberghe.8';
			package: 'Zinc-Patch-HTTPSocket' with: 'Zinc-Patch-HTTPSocket-StephaneDucasse.2';
			package: 'Zodiac' with: 'Zodiac-Core-SvenVanCaekenberghe.11'. ].

	spec for: #'gemstone' do: [
		spec blessing: #'release'.
		spec 
			package: 'Zinc-GemstonePreliminary' with: 'Zinc-GemstonePreliminary-PaulDeBruicker.2';
			package: 'Zinc-Gemstone' with: 'Zinc-Gemstone-PaulDeBruicker.14';
			package: 'SocketStream' with: 'SocketStream-PaulDeBruicker.12'. ].
