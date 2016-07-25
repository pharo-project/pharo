versions
version34: spec 
        <version: '3.4' imports: #('2.3-baseline')>

       spec for: #common do: [
			spec description: '2015-04-27T23:43:23.908271+02:00'.
			spec blessing: #development.

			spec package: 'Athens-Core' with: 'Athens-Core-SeanDeNigris.51'.
			spec package: 'Athens-Text' with: 'Athens-Text-NicolaiHess.16'.
			spec package: 'Athens-Balloon' with: 'Athens-Balloon-MarcusDenker.20'.
			spec package: 'Athens-Morphic' with: 'Athens-Morphic-NicolaiHess.47'.
			spec package: 'Athens-CairoPools' with: 'Athens-CairoPools-NicolaiHess.14'.
			spec package: 'Athens-Cairo' with: 'Athens-Cairo-NicolaiHess.87'.
			spec package: 'Athens-Examples' with: 'Athens-Examples-TudorGirba.43'.
			spec package: 'Athens-Tutorial' with: 'Athens-Tutorial-StephaneDucasse.10'.
			spec package: 'Athens-SVG' with: 'Athens-SVG-StephaneDucasse.12' ]