versions
version29: spec 
        <version: '2.9' imports: #('2.3-baseline')>

       spec for: #common do: [
			spec description: '2015-02-17T23:22:38.219799+01:00'.
			spec blessing: #development.

			spec package: 'Athens-Core' with: 'Athens-Core-NicolaiHess.46'.
			spec package: 'Athens-Text' with: 'Athens-Text-NicolaiHess.16'.
			spec package: 'Athens-Balloon' with: 'Athens-Balloon-NicolaiHess.19'.
			spec package: 'Athens-Morphic' with: 'Athens-Morphic-NicolaiHess.43'.
			spec package: 'Athens-CairoPools' with: 'Athens-CairoPools-NicolaiHess.14'.
			spec package: 'Athens-Cairo' with: 'Athens-Cairo-NicolaiHess.84'.
			spec package: 'Athens-Examples' with: 'Athens-Examples-NicolaiHess.41'.
			spec package: 'Athens-Tutorial' with: 'Athens-Tutorial-StephaneDucasse.10'.
			spec package: 'Athens-SVG' with: 'Athens-SVG-StephaneDucasse.12' ]