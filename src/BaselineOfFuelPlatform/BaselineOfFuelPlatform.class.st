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
	"Ensure that on development platforms the current platform is correct,
	e.g. when we add a new platform the loaded one could be FLPharo7Platform
	but we need it to be FLPharo8Platform after loading."

	(Smalltalk at: #FLPlatform) reset
]

{ #category : #baselines }
BaselineOfFuelPlatform >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec postLoadDoIt: #addPostloadHacks.

		spec package: 'Fuel-Platform-Core' ]
]
