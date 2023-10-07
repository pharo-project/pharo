Class {
	#name : 'BaselineOfReflectionMirrors',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfReflectionMirrors',
	#package : 'BaselineOfReflectionMirrors'
}

{ #category : 'baselines' }
BaselineOfReflectionMirrors >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'ReflectionMirrors-Primitives-Tests' ]
]
