Class {
	#name : 'MetacelloAllowProjectDowngrade',
	#superclass : 'MetacelloResolveProjectUpgrade',
	#category : 'Deprecated14',
	#package : 'Deprecated14'
}

{ #category : 'testing' }
MetacelloAllowProjectDowngrade class >> isDeprecated [
	"This exception cannot be raised anymore since we cannot load ConfigurationOf."

	^ true
]
