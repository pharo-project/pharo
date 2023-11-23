Class {
	#name : 'BaselineOfExternalProjectLoader',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfExternalProjectLoader',
	#package : 'BaselineOfExternalProjectLoader'
}

{ #category : 'baselines' }
BaselineOfExternalProjectLoader >> baseline: spec [
	<baseline>
	| repository |

	repository := self packageRepositoryURL.

	spec for: #common do: [ 
		spec package: 'ExternalProjectLoader'.
		
		spec group: 'Core' with: #('ExternalProjectLoader').
		spec group: 'default' with: #('Core') ]
]
