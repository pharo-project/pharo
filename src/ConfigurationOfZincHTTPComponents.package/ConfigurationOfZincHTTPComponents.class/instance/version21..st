versions
version21: spec
	<version: '2.1' imports: #('2.1-baseline')>
	spec for: #common do: [ 
		spec 
			blessing: #release;
			description: 'Update to latest package versions';
			author: 'SvenVanCaekenberghe';
			package: 'Zinc-Character-Encoding-Core' with: 'Zinc-Character-Encoding-Core-SvenVanCaekenberghe.2';
			package: 'Zinc-Character-Encoding-Tests' with: 'Zinc-Character-Encoding-Tests-SvenVanCaekenberghe.2';
			package: 'Zinc-Resource-Meta-Core' with: 'Zinc-Resource-Meta-Core-SvenVanCaekenberghe.3';
			package: 'Zinc-Resource-Meta-Tests' with: 'Zinc-Resource-Meta-Tests-SvenVanCaekenberghe.3';
			package: 'Zinc-HTTP' with: 'Zinc-HTTP-SvenVanCaekenberghe.328';
			package: 'Zinc-Tests' with: 'Zinc-Tests-SvenVanCaekenberghe.169';
			package: 'Zinc-AWS' with: 'Zinc-AWS-SvenVanCaekenberghe.16';
			package: 'Zinc-REST' with: 'Zinc-REST-SvenVanCaekenberghe.8';
			package: 'Zinc-WebDAV' with: 'Zinc-WebDAV-SvenVanCaekenberghe.3';
			package: 'Zinc-WebSocket-Core' with: 'Zinc-WebSocket-Core-SvenVanCaekenberghe.23';
			package: 'Zinc-WebSocket-Tests' with: 'Zinc-WebSocket-Tests-SvenVanCaekenberghe.7';
			package: 'Zinc-WWS-Client' with: 'Zinc-WWS-Client-SvenVanCaekenberghe.2';
			package: 'Zinc-WWS-Server' with: 'Zinc-WWS-Server-SvenVanCaekenberghe.8';
			package: 'Zinc-Zodiac' with: 'Zinc-Zodiac-SvenVanCaekenberghe.23';
			package: 'Zinc-Patch-HTTPSocket' with: 'Zinc-Patch-HTTPSocket-SvenVanCaekenberghe.1';
			package: 'Zodiac-Core' with: 'Zodiac-Core-SvenVanCaekenberghe.27';
			package: 'Zodiac-Tests' with: 'Zodiac-Tests-SvenVanCaekenberghe.10' ].
	spec for: #'pharo1.x' do: [ 
		spec 
			package: 'Zinc-FileSystem-Legacy' with: 'Zinc-FileSystem-Legacy-SvenVanCaekenberghe.3';
			package: 'Zinc-Pharo-Forward-Compatibility' with: 'Zinc-Pharo-Forward-Compatibility-SvenVanCaekenberghe.1' ].
	spec for: #'pharo2.x' do: [ 
		spec package: 'Zinc-FileSystem' with: 'Zinc-FileSystem-SvenVanCaekenberghe.7' ].
	spec for: #gemstone do: [ 
		spec blessing: #release.
		spec
			package: 'Zinc-GemstonePreliminary' with: 'Zinc-GemstonePreliminary-PaulDeBruicker.2';
			package: 'Zinc-Gemstone' with: 'Zinc-Gemstone-PaulDeBruicker.14';
			package: 'SocketStream' with: 'SocketStream-PaulDeBruicker.12' ]
