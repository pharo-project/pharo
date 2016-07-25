versions
version25: spec 
        <version: '2.5' imports: #('2.3-baseline')>

       spec for: #common do: [
 	 	  spec description: 'Update 31 Jan 2014'.
               spec blessing: #development.

			spec package: 'Athens-Core' with: 'Athens-Core-JochenRick.40'.
			spec package: 'Athens-Text' with: 'Athens-Text-IgorStasenko.9'.
			spec package: 'Athens-Balloon' with: 'Athens-Balloon-IgorStasenko.15'.
			spec package: 'Athens-Morphic' with: 'Athens-Morphic-JochenRick.28'.

			spec package: 'Athens-CairoPools' with: 'Athens-CairoPools-IgorStasenko.10'.

			spec package: 'Athens-Cairo' with: 'Athens-Cairo-IgorStasenko.61'.
			spec package: 'Athens-Examples' with: 'Athens-Examples-IgorStasenko.26'.
			spec package: 'Athens-Tutorial' with: 'Athens-Tutorial-IgorStasenko.6'.
			spec package: 'Athens-SVG' with: 'Athens-SVG-JochenRick.8'
			
                ].