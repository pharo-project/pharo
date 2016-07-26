versions
version17: spec
	<version: '1.7' imports: #('1.7-baseline')>
	spec
		for: #common
		do: [ 
			spec blessing: #release.
			spec description: '
- update to latest package versions
- add  FileSystem-Legacy to Pharo 1.x and FileSystem to Pharo 2.x'.
			spec author: 'SeanDeNigris'.
			spec timestamp: '7/4/2012 12:12-4:00'.
			spec
				package: 'Zinc-HTTP' with: 'Zinc-HTTP-SvenVanCaekenberghe.282';
				package: 'Zinc-Tests' with: 'Zinc-Tests-SvenVanCaekenberghe.147';
				package: 'Zinc-AWS' with: 'Zinc-AWS-SvenVanCaekenberghe.15';
				package: 'Zinc-REST' with: 'Zinc-REST-SvenVanCaekenberghe.5';
				package: 'Zinc-WebDAV' with: 'Zinc-WebDAV-SvenVanCaekenberghe.3';
				package: 'Zinc-WWS-Client' with: 'Zinc-WWS-Client-SvenVanCaekenberghe.2';
				package: 'Zinc-WWS-Server' with: 'Zinc-WWS-Server-SvenVanCaekenberghe.7';
				package: 'Zinc-Zodiac' with: 'Zinc-Zodiac-SvenVanCaekenberghe.14';
				package: 'Zinc-Patch-HTTPSocket' with: 'Zinc-Patch-HTTPSocket-StephaneDucasse.2';
				package: 'Zodiac-Core' with: 'Zodiac-Core-SvenVanCaekenberghe.18' ].
	spec for: #'pharo1.x' do: [ spec package: 'Zinc-FileSystem-Legacy' with: 'Zinc-FileSystem-Legacy-SvenVanCaekenberghe.1' ].
	spec for: #'pharo2.x' do: [ spec package: 'Zinc-FileSystem' with: 'Zinc-FileSystem-SeanDeNigris.2' ].
	spec
		for: #gemstone
		do: [ 
			spec blessing: #release.
			spec
				package: 'Zinc-GemstonePreliminary' with: 'Zinc-GemstonePreliminary-PaulDeBruicker.2';
				package: 'Zinc-Gemstone' with: 'Zinc-Gemstone-PaulDeBruicker.14';
				package: 'SocketStream' with: 'SocketStream-PaulDeBruicker.12' ]