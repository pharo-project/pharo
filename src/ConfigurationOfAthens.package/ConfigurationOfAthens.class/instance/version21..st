versions
version21: spec 
        <version: '2.1' imports: #('2.1-baseline')>

        spec for: #common do: [
                spec blessing: #development.

			spec package: 'Athens-Core' with: 'Athens-Core-IgorStasenko.32'.
			spec package: 'Athens-Text' with: 'Athens-Text-IgorStasenko.5'.
			spec package: 'Athens-Balloon' with: 'Athens-Balloon-IgorStasenko.10'.
			spec package: 'Athens-Morphic' with: 'Athens-Morphic-IgorStasenko.18'.
			spec package: 'Athens-CairoPools' with: 'Athens-CairoPools-IgorStasenko.6'.
			spec package: 'Athens-Cairo' with: 'Athens-Cairo-IgorStasenko.43'.
			spec package: 'Athens-PangoCairo' with: 'Athens-PangoCairo-FernandoOlivero.11'.
			spec package: 'Athens-Examples' with: 'Athens-Examples-IgorStasenko.22'.
			
			spec package: 'Athens-Tutorial' with: 'Athens-Tutorial-IgorStasenko.4'.
			spec package: 'Athens-SVG' with: 'Athens-SVG-IgorStasenko.7'
			
                ].