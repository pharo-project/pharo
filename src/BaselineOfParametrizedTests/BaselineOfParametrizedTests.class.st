Class {
	#name : 'BaselineOfParametrizedTests',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfParametrizedTests',
	#package : 'BaselineOfParametrizedTests'
}

{ #category : 'development support' }
BaselineOfParametrizedTests class >> DevelopmentSupport [

"See the methods in the 'development support' category on the class-side of MetacelloBaseConfiguration. Decide what development support methods you would like to use and copy them the the class-side of your configuration."
	<apiDocumentation>
]

{ #category : 'metacello tool support' }
BaselineOfParametrizedTests class >> isMetacelloConfig [
	"Answer true and the Metacello tools will operate on you"
	
	^true
]

{ #category : 'loading' }
BaselineOfParametrizedTests class >> load [
	"Load the #stable version defined for this platform. The #stable version is the version that is recommended to be used on this platform."

	"self load"

	<apiDocumentation>
	^(self project version: #stable) load
]

{ #category : 'loading' }
BaselineOfParametrizedTests class >> loadBleedingEdge [
	"Load the latest versions of the mcz files defined for this project. It is not likely that the #bleedingEdge has been tested."

	"self loadBleedingEdge"

	<apiDocumentation>
	^(self project version: #bleedingEdge) load
]

{ #category : 'loading' }
BaselineOfParametrizedTests class >> loadDevelopment [
	"Load the #development version defined for this platform. The #development version will change over time and is not expected to be stable."

	"self loadDevelopment"

	<apiDocumentation>
	^(self project version: #development) load
]

{ #category : 'baselines' }
BaselineOfParametrizedTests >> baseline: spec [
	<baseline>
	spec for: #common do: [ spec package: #ParametrizedTests ]
]
