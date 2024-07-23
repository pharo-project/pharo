Class {
	#name : 'BaselineOfReflectivity',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfReflectivity',
	#package : 'BaselineOfReflectivity'
}

{ #category : 'baselines' }
BaselineOfReflectivity >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec postLoadDoIt: #postload:package:.

		spec
			baseline: 'Instrumentation'
			with: [ spec repository: (self packageRepositoryURLForSpec: spec) ];
			package: 'Reflectivity' with: [ spec requires: 'Instrumentation' ];
			package: 'Reflectivity-Tools'
			with: [ spec requires: 'Reflectivity' ];
			package: 'Reflectivity-Tools-Tests'
			with: [ spec requires: 'Reflectivity-Tools' ];
			package: 'Reflectivity-Examples'
			with: [ spec requires: 'Reflectivity' ];
			package: 'Reflectivity-Tests'
			with: [ spec requires: #( Reflectivity 'Reflectivity-Tools' ) ].
		"Reflectivity requires using code instrumentation"

		spec
			group: 'default' with: #( 'Reflectivity' );
			group: 'tools'
			with: #( 'Reflectivity-Tools' 'Reflectivity-Tools-Tests' );
			group: 'tests'
			with:
				#( 'default' 'tools' 'Reflectivity-Tests' 'Reflectivity-Examples' ) ]
]

{ #category : 'baselines' }
BaselineOfReflectivity >> postload: loader package: packageSpec [

	ASTCache cacheMissStrategy: RFReflectivityASTCacheStrategy new
]
