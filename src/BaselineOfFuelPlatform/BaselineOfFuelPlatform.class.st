"
We use a separate baseline to make it simpler to manage the platform classes. Using a separate baseline also makes it easy to load all the packages at once when we need to update them.
"
Class {
	#name : #BaselineOfFuelPlatform,
	#superclass : #BaselineOf,
	#category : #BaselineOfFuelPlatform
}

{ #category : #adding }
BaselineOfFuelPlatform >> addPostloadHacks [
	| platformClass |
	platformClass := Smalltalk at: #FLPlatform.
	
	"Ensure that on development platforms the current platform is correct,
	e.g. when we add a new platform the loaded one could be FLPharo7Platform
	but we need it to be FLPharo8Platform after loading."
	platformClass reset.
	
	"Simply perform #addHacks again. We've now loaded classes that weren't
	there before and some of those also need extensions" 
	platformClass current addHacks
]

{ #category : #baselines }
BaselineOfFuelPlatform >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec postLoadDoIt: #addPostloadHacks.

		spec
			package: 'Fuel-Platform-Core';
			package: 'Fuel-Platform-Pharo-Core' with: [ spec requires: 'Fuel-Platform-Core' ] ]
]
