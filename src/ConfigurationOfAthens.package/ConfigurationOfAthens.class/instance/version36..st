versions
version36: spec 
        <version: '3.6' imports: #('2.3-baseline')>

       spec for: #common do: [
			spec description: 'issue 15760'.
			spec blessing: #development.

			spec package: 'Athens-Core' with: 'Athens-Core-maxmattone.54'.
			spec package: 'Athens-Text' with: 'Athens-Text-NicolaiHess.16'.
			spec package: 'Athens-Balloon' with: 'Athens-Balloon-MarcusDenker.20'.
			spec package: 'Athens-Morphic' with: 'Athens-Morphic-AlainPlantec.55'.
			spec package: 'Athens-CairoPools' with: 'Athens-CairoPools-NicolaiHess.14'.
			spec package: 'Athens-Cairo' with: 'Athens-Cairo-Nicolaihess.88'.
			spec package: 'Athens-Examples' with: 'Athens-Examples-TudorGirba.43'.
			spec package: 'Athens-Tutorial' with: 'Athens-Tutorial-StephaneDucasse.10'.
			spec package: 'Athens-SVG' with: 'Athens-SVG-StephaneDucasse.12' ]