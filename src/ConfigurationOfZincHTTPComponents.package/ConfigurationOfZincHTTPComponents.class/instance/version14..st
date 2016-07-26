versions
version14: spec
	<version: '1.4' imports: #('1.4-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: 'update to latest set of packages.'.
		spec author: 'FrancoisStephany'.
		spec timestamp: '12/12/2011 11:52'.
		spec 
			package: 'Zinc-HTTP' with: 'Zinc-HTTP-SvenVanCaekenberghe.227';
			package: 'Zinc-Tests' with: 'Zinc-Tests-SvenVanCaekenberghe.118';
			package: 'Zinc-AWS' with: 'Zinc-AWS-SvenVanCaekenberghe.15';
			package: 'Zinc-REST' with: 'Zinc-REST-SvenVanCaekenberghe.5';
			package: 'Zinc-Zodiac' with: 'Zinc-Zodiac-SvenVanCaekenberghe.10';
			package: 'Zinc-Patch-HTTPSocket' with: 'Zinc-Patch-HTTPSocket-StephaneDucasse.2';
			package: 'Zodiac' with: 'Zodiac-Core-SvenVanCaekenberghe.16'. ].

	spec for: #'gemstone' do: [
		spec blessing: #'release'.
		spec 
			package: 'Zinc-GemstonePreliminary' with: 'Zinc-GemstonePreliminary-PaulDeBruicker.2';
			package: 'Zinc-Gemstone' with: 'Zinc-Gemstone-PaulDeBruicker.14';
			package: 'SocketStream' with: 'SocketStream-PaulDeBruicker.12'. ].
