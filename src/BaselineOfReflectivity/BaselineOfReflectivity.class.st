Class {
	#name : #BaselineOfReflectivity,
	#superclass : #BaselineOf,
	#category : #BaselineOfReflectivity
}

{ #category : #baselines }
BaselineOfReflectivity >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ 
			spec postLoadDoIt: #'postload:package:'.
			
			spec
				package: 'Reflectivity';
				package: 'Reflectivity-Tests' with: [ spec requires: 'Reflectivity' ];
				package: 'Reflectivity-Examples' with: [ spec requires: 'Reflectivity' ];
				package: 'Reflectivity-VariableBreakpoint' with: [ spec requires: 'Reflectivity' ].
		
		spec
				group: 'default' with: #('Reflectivity');
				group: 'tests' with: #('default' 'Reflectivity-Tests' 'Reflectivity-Examples' 'Reflectivity-VariableBreakpoint') ]
		
	
]

{ #category : #baselines }
BaselineOfReflectivity >> postload: loader package: packageSpec [
	ASTCache cacheMissStrategy: RFReflectivityASTCacheStrategy new.
]
