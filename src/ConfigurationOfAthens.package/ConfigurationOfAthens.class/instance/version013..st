versions
version013: spec 
        <version: '0.1.3' imports: #('1.0-baseline')>

        spec for: #common do: [
               spec blessing: #development.

			spec project: 'ConfigurationOfNativeBoost default'  with: '1.0.9'.

			spec package: 'NativeBoost-Installer' with: 'NativeBoost-Installer-IgorStasenko.24'.
			spec package: 'Athens-Core' with: 'Athens-Core-FernandoOlivero.8'.
			spec package: 'Athens-Text' with: 'Athens-Text-IgorStasenko.2'.
			spec package: 'Athens-Balloon' with: 'Athens-Balloon-IgorStasenko.4'.
			spec package: 'Athens-Morphic' with: 'Athens-Morphic-IgorStasenko.2'.
			spec package: 'Athens-CairoPools' with: 'Athens-CairoPools-IgorStasenko.2'.
			spec package: 'Athens-Cairo' with: 'Athens-Cairo-FernandoOlivero.9'.
			spec package: 'Athens-PangoCairo' with: 'Athens-PangoCairo-FernandoOlivero.5'.
			spec package: 'Athens-Examples' with: 'Athens-Examples-FernandoOlivero.5'.
	].