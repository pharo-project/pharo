baselines-helpers
groupsForBaseline200PharoCommon: spec
	spec for: #pharo do: [              
		spec
			package: 'FuelPreview' with: [ spec requires: #( 'FuelDebug' 'Roassal')];
			package: 'FuelMetalevel' with: [ spec requires: #( 'Fuel' )];
			package: 'FuelMetalevelTests' with: [ spec requires: #('FuelMetalevel' 'FuelTests' )].
		
		self roassal: spec.

		spec
			group: 'CoreWithExtras' with: #('Core'  'FuelMetalevel' 'FuelProgressUpdate');
			group: 'Tests' with: #('FuelTests' );
			group: 'DevelopmentGroup' with: #('CoreWithExtras' 'Tests' 'FuelMetalevelTests' 'FuelDebug'). 
	]