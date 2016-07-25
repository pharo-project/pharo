versions
version32: spec 
        <version: '3.2' imports: #('2.3-baseline')>

       spec for: #common do: [
			spec description: '2015-03-24T15:04:20.991359+01:00'.
			spec blessing: #development.

			spec package: 'Athens-Core' with: 'Athens-Core-MarcusDenker.50'.
			spec package: 'Athens-Text' with: 'Athens-Text-NicolaiHess.16'.
			spec package: 'Athens-Balloon' with: 'Athens-Balloon-MarcusDenker.20'.
			spec package: 'Athens-Morphic' with: 'Athens-Morphic-NicolaiHess.44'.
			spec package: 'Athens-CairoPools' with: 'Athens-CairoPools-NicolaiHess.14'.
			spec package: 'Athens-Cairo' with: 'Athens-Cairo-jeanbaptistearnaud.86'.
			spec package: 'Athens-Examples' with: 'Athens-Examples-TudorGirba.43'.
			spec package: 'Athens-Tutorial' with: 'Athens-Tutorial-StephaneDucasse.10'.
			spec package: 'Athens-SVG' with: 'Athens-SVG-StephaneDucasse.12' ]