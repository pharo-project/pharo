versions
version16: spec
	<version: '1.6' imports: #('1.5-baseline')>
	spec
		for: #common
		do: [ 
			spec blessing: #release.
			spec description: 'update to latest set of packages.'.
			spec author: 'PaulDeBruicker'.
			spec timestamp: '6/6/2012 13:12-7:00'.
			spec
				package: 'Zinc-HTTP' with: 'Zinc-HTTP-SvenVanCaekenberghe.280';
				package: 'Zinc-Tests' with: 'Zinc-Tests-SvenVanCaekenberghe.144';
				package: 'Zinc-AWS' with: 'Zinc-AWS-SvenVanCaekenberghe.15';
				package: 'Zinc-REST' with: 'Zinc-REST-SvenVanCaekenberghe.5';
				package: 'Zinc-WebDAV' with: 'Zinc-WebDAV-SvenVanCaekenberghe.3';
				package: 'Zinc-WWS-Client' with: 'Zinc-WWS-Client-SvenVanCaekenberghe.2';
				package: 'Zinc-WWS-Server' with: 'Zinc-WWS-Server-SvenVanCaekenberghe.7';
				package: 'Zinc-Zodiac' with: 'Zinc-Zodiac-SvenVanCaekenberghe.12';
				package: 'Zinc-Patch-HTTPSocket' with: 'Zinc-Patch-HTTPSocket-StephaneDucasse.2';
				package: 'Zodiac-Core' with: 'Zodiac-Core-SvenVanCaekenberghe.17' ].
	spec
		for: #gemstone
		do: [ 
			spec blessing: #release.
			spec
				package: 'Zinc-GemstonePreliminary' with: 'Zinc-GemstonePreliminary-PaulDeBruicker.2';
				package: 'Zinc-Gemstone' with: 'Zinc-Gemstone-PaulDeBruicker.14';
				package: 'SocketStream' with: 'SocketStream-PaulDeBruicker.12' ]