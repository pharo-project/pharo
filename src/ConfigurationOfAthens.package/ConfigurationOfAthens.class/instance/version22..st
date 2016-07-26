versions
version22: spec 
        <version: '2.2' imports: #('2.1-baseline')>

       spec for: #common do: [
 	 	  spec description: 'update after nativeboost NBExternalStructure refactoring ConfigurationOfNativeBoost 2.2'.
               spec blessing: #development.

			spec package: 'Athens-Core' with: 'Athens-Core-IgorStasenko.35'.
			spec package: 'Athens-Text' with: 'Athens-Text-IgorStasenko.6'.
			spec package: 'Athens-Balloon' with: 'Athens-Balloon-IgorStasenko.10'.
			spec package: 'Athens-Morphic' with: 'Athens-Morphic-IgorStasenko.20'.
			spec package: 'Athens-CairoPools' with: 'Athens-CairoPools-IgorStasenko.8'.
			spec package: 'Athens-Cairo' with: 'Athens-Cairo-IgorStasenko.48'.
			spec package: 'Athens-PangoCairo' with: 'Athens-PangoCairo-FernandoOlivero.11'.
			spec package: 'Athens-Examples' with: 'Athens-Examples-IgorStasenko.25'.
			
			spec package: 'Athens-Tutorial' with: 'Athens-Tutorial-IgorStasenko.6'.
			spec package: 'Athens-SVG' with: 'Athens-SVG-IgorStasenko.7'
			
                ].