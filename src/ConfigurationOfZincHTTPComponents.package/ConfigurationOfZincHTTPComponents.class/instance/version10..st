versions
version10: spec 
	<version: '1.0' imports: #('1.0-baseline') >
	
	spec for: #common do: [
		spec blessing: #release.
		spec 
			package: 'Zinc-HTTP' with: 'Zinc-HTTP-PaulDeBruicker.140';
			package: 'Zinc-Tests' with: 'Zinc-Tests-SvenVanCaekenberghe.75';
			package: 'Zinc-AWS' with:'Zinc-AWS-SvenVanCaekenberghe.7';
			package: 'Zinc-Patch-HTTPSocket' with:'Zinc-Patch-HTTPSocket-SvenVanCaekenberghe.1';
			package: 'Zinc-Seaside' with:'Zinc-Seaside-PaulDeBruicker.13';
			project:'Seaside30' with: '3.0.4' ].
		
	spec for: #gemstone do:[
		spec blessing: #release.
		spec 
			package: 'Zinc-GemstonePreliminary' with: 'Zinc-GemstonePreliminary-PaulDeBruicker.2';
			package: 'Zinc-Gemstone' with: 'Zinc-Gemstone-PaulDeBruicker.8';
			package:'SocketStream' with:'SocketStream-PaulDeBruicker.12';
			package:'Seaside-GemStone-Adaptors-Zinc' with: 'Seaside-GemStone-Adaptors-Zinc-PaulDeBruicker.3'   		
		]