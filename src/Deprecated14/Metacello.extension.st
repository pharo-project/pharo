Extension { #name : 'Metacello' }

{ #category : '*Deprecated14' }
Metacello >> configuration: projectName [

	self deprecated: 'ConfigurationOf loading is not supported since Pharo 12. Only Baselines are used now.'
]

{ #category : '*Deprecated14' }
Metacello >> project: projectName [

	self deprecated: 'Projects are not supported anymore. Only Baselines are used now.'
]

{ #category : '*Deprecated14' }
Metacello >> version: versionString [

	self deprecated:
		'Configurations are not supported anymore, only baseline can be used as dependencies and project managed with baseline have their version managed by the repositories.'
]
