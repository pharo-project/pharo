Extension { #name : #IceRepository }

{ #category : #'*Iceberg-Plugin' }
IceRepository >> pluginManager [
	^ IcePluginManager managerFor: self
]
