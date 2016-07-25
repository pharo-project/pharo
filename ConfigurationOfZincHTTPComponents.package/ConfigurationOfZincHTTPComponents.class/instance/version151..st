versions
version151: spec
	<version: '1.5.1' imports: #('1.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #release.
		spec description: 'fixed Zodiac-Core package name; updated to latest packages; all tests pass on Pharo 1.4'.
		spec author: 'SeanDeNigris'.
		spec timestamp: '6/7/2012 00:23'.
		spec 
			package: 'Zodiac-Core' with: 'Zodiac-Core-SvenVanCaekenberghe.17';
			package: 'Zinc-HTTP' with: 'Zinc-HTTP-SvenVanCaekenberghe.280';
			package: 'Zinc-Patch-HTTPSocket' with: 'Zinc-Patch-HTTPSocket-StephaneDucasse.2';
			package: 'Zinc-AWS' with: 'Zinc-AWS-SvenVanCaekenberghe.15';
			package: 'Zinc-REST' with: 'Zinc-REST-SvenVanCaekenberghe.5';
			package: 'Zinc-Zodiac' with: 'Zinc-Zodiac-SvenVanCaekenberghe.12';
			package: 'Zinc-WebDAV' with: 'Zinc-WebDAV-SvenVanCaekenberghe.3';
			package: 'Zinc-WWS-Server' with: 'Zinc-WWS-Server-SvenVanCaekenberghe.7';
			package: 'Zinc-WWS-Client' with: 'Zinc-WWS-Client-SvenVanCaekenberghe.2';
			package: 'Zinc-Tests' with: 'Zinc-Tests-SvenVanCaekenberghe.144'. ].