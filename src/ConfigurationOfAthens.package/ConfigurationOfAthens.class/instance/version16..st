versions
version16: spec 
        <version: '1.6' imports: #('1.1-baseline')>

        spec for: #common do: [
                spec blessing: #development.
			spec project: 'ConfigurationOfNativeBoost default'  with: '1.8'.

			spec package: 'Athens-Core' with: 'Athens-Core-IgorStasenko.25'.
			spec package: 'Athens-Text' with: 'Athens-Text-IgorStasenko.5'.
			spec package: 'Athens-Balloon' with: 'Athens-Balloon-IgorStasenko.7'.
			spec package: 'Athens-Morphic' with: 'Athens-Morphic-IgorStasenko.9'.
			spec package: 'Athens-CairoPools' with: 'Athens-CairoPools-IgorStasenko.5'.
			spec package: 'Athens-Cairo' with: 'Athens-Cairo-IgorStasenko.34'.
			spec package: 'Athens-PangoCairo' with: 'Athens-PangoCairo-FernandoOlivero.11'.
			spec package: 'Athens-Examples' with: 'Athens-Examples-DamienPollet.20'.
                ].