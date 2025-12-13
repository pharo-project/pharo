Extension { #name : 'Metacello' }

{ #category : '*Deprecated14' }
Metacello >> configuration: projectName [

	self error: 'Configurations are not supported anymore. Only Baselines are used now.'
]

{ #category : '*Deprecated14' }
Metacello >> project: projectName [

	self error: 'Projects are not supported anymore. Only Baselines are used now.'
]

{ #category : '*Deprecated14' }
Metacello >> version: versionString [

	self error:
		'Configurations are not supported anymore, only baseline can be used as dependencies and project managed with baseline have their version managed by the repositories.'
]
