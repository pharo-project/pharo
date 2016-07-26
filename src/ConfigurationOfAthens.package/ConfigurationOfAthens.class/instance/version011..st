versions
version011: spec 
        <version: '0.1.1' imports: #('1.0-baseline')>

        spec for: #common do: [
                spec blessing: #development.
			spec 
				project: 'ConfigurationOfNativeBoost default'  with: '1.0.6';
				package: 'NativeBoost-Installer' with: 'NativeBoost-Installer-IgorStasenko.21'.
	
			spec package: 'Athens-Core' with: 'Athens-Core-sig.5'.
			spec package: 'Athens-Text' with: 'Athens-Text-IgorStasenko.1'.
			spec package: 'Athens-Balloon' with: 'Athens-Balloon-sig.3'.
			spec package: 'Athens-Morphic' with: 'Athens-Morphic-IgorStasenko.1'.
			spec package: 'Athens-CairoPools' with: 'Athens-CairoPools-IgorStasenko.2'.
			spec package: 'Athens-Cairo' with: 'Athens-Cairo-IgorStasenko.7'.
			spec package: 'Athens-PangoCairo' with: 'Athens-PangoCairo-FernandoOlivero.3'.
			spec package: 'Athens-Examples' with: 'Athens-Examples-sig.2'.
                ].