"
Baseline managing the Athens project for the Pharo bootstrap.

Load type: 
- linear
"
Class {
	#name : #BaselineOfAthens,
	#superclass : #BaselineOf,
	#category : #BaselineOfAthens
}

{ #category : #baseline }
BaselineOfAthens >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ 
			self flag: #todo. "We need to cut cyclic dependencies and add requirements to the packages."
			
			"Packages"
			spec package: 'Athens-Core'. "There is a cyclic dependency between Core and Morphic"
			spec package: 'Athens-Text'.
			spec package: 'Athens-Balloon'.
			spec package: 'Athens-Morphic'.
			spec package: 'Athens-CairoPools'.
			spec package: 'Athens-Cairo'.
			spec package: 'Athens-Examples'.
			spec package: 'Athens-Tests-Cairo'.
			
			"Groups"
			spec
				group: 'Core' with: #('Basic' 'Athens-Text' 'Athens-Balloon' 'Athens-Morphic');
				group: 'Basic' with: #('Athens-Core' 'Athens-CairoPools' 'Athens-Cairo');
				group: 'Examples' with: #('Athens-Examples'); "Test and Examples are not yet working. We need to add the requirements between packages to get them working"
				group: 'Tests' with: #('Athens-Tests-Cairo') ]
]
