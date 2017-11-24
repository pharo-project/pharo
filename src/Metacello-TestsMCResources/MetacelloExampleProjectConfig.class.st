Class {
	#name : #MetacelloExampleProjectConfig,
	#superclass : #MetacelloMCTestsAbstractConfig,
	#category : #'Metacello-TestsMCResources'
}

{ #category : #baselines }
MetacelloExampleProjectConfig >> baseline10: spec [ 
	<version: '1.0-baseline'>
	
	spec for: #common do: [
		spec 
			package: 'Example-Core';
			package: 'Example-AddOn' with: [ spec requires: #('Example-Core' ) ];
			package: 'Example-Tests' with: [ spec requires: #('Example-AddOn' ) ];
			package: 'Example-TestsUI' with: [ spec requires: #('Example-UI' 'Example-Tests' ) ];
			package: 'Example-UI' with: [ spec requires: #('Example-AddOn' ) ]].

]
