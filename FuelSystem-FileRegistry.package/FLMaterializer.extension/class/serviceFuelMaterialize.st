*FuelSystem-FileRegistry
serviceFuelMaterialize
	^ SimpleServiceEntry 
		provider: self 
		label: 'Materialize Fuel file'
		selector: #materializeFromFileNamed:
		description: 'Materialize objects previously serialized with Fuel'
		buttonLabel: 'materialize'