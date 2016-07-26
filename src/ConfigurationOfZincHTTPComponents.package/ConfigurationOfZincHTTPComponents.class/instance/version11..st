versions
version11: spec 
	<version: '1.1' imports: #('1.1-baseline') >
	
	spec for: #common do: [
		spec blessing: #release.
		spec 
			package: 'Zinc-HTTP' with: 'Zinc-HTTP-SvenVanCaekenberghe.162';
			package: 'Zinc-Tests' with: 'Zinc-Tests-SvenVanCaekenberghe.84';
			package: 'Zinc-AWS' with:'Zinc-AWS-SvenVanCaekenberghe.7';
			package: 'Zinc-Zodiac' with:'Zinc-Zodiac-SvenVanCaekenberghe.4';
			package: 'Zinc-Patch-HTTPSocket' with:'Zinc-Patch-HTTPSocket-SvenVanCaekenberghe.1';
			package: 'Zinc-Seaside' with:'Zinc-Seaside-SvenVanCaekenberghe.16';
			package: 'Zodiac' with: 'Zodiac-Core-SvenVanCaekenberghe.8' ;
			project:'Seaside30' with: '3.0.5'
			].
		
	spec for: #gemstone do:[
		spec blessing: #release.
		spec 
			package: 'Zinc-GemstonePreliminary' with: 'Zinc-GemstonePreliminary-PaulDeBruicker.2';
			package: 'Zinc-Gemstone' with: 'Zinc-Gemstone-PaulDeBruicker.14';
			package:'SocketStream' with:'SocketStream-PaulDeBruicker.12';
			package:'Seaside-GemStone-Adaptors-Zinc' with: 'Seaside-GemStone-Adaptors-Zinc-PaulDeBruicker.3'   		
		]