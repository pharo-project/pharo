Class {
	#name : #BaselineOfFuel,
	#superclass : #BaselineOf,
	#category : #BaselineOfFuel
}

{ #category : #baselines }
BaselineOfFuel >> baseline: spec [ 
    
	<baseline>
	
	spec package: 'Fuel-Core'.
	spec package: 'Fuel-Platform-Core'.
	spec package: 'Fuel-Platform-Pharo-Core'.
	spec package: 'Fuel-Platform-Pharo-07'.

	spec package: 'Fuel-Platform-Pharo-Core-Tests'	.
	spec package: 'Fuel-Tests-Core'.
	
	spec package: 'Fuel-Tools-Pharo'.

	spec 
		group: 'Core' with: #('Fuel-Core' 'Fuel-Platform-Core' 'Fuel-Platform-Pharo-Core' 'Fuel-Platform-Pharo-07');
		group: 'Tools' with: #('Fuel-Tools-Pharo');
		group: 'Tests' with: #('Fuel-Platform-Pharo-Core-Tests' 'Fuel-Tests-Core');
		group: 'default' with: #('Core' )
]
