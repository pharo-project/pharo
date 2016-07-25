versions-1
version19AlphaForTanker: spec 
	<version: '1.9-alphaForTanker' imports: #('1.9-alphaForTanker-baseline') >

	spec for: #pharo do: [
		spec blessing: #beta.
		spec description: ' just a fixed version for Tanker'.

		spec 
			package: 'Fuel' with: 'Fuel-MarianoMartinezPeck.718';
			package: 'FuelTests' with: 'FuelTests-MarianoMartinezPeck.286';
			package: 'FuelMetalevel' with: 'FuelMetalevel-MarianoMartinezPeck.60';
			package: 'FuelMetalevelTests' with: 'FuelMetalevelTests-MarianoMartinezPeck.46'.

		].