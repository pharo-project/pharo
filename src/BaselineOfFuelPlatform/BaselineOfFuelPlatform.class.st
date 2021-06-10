"
We use a separate baseline to make it simpler to manage the platform classes. Using a separate baseline also makes it easy to load all the packages at once when we need to update them.
"
Class {
	#name : #BaselineOfFuelPlatform,
	#superclass : #BaselineOf,
	#instVars : [
		'customProjectAttributes'
	],
	#category : #BaselineOfFuelPlatform
}

{ #category : #accessing }
BaselineOfFuelPlatform >> addCustomProjectAttribute: aSymbol [
	customProjectAttributes := self customProjectAttributes copyWith: aSymbol
]

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
  spec
	for: #common
	do: [		
		spec
			postLoadDoIt: #addPostloadHacks;
			package: 'Fuel-Platform-Core'.
			
		spec
			group: 'default'
			with: #('Fuel-Platform-Core' 'FuelPlatformCurrentGroup') ].
	
	self 
		pharoPackagesAndGroups: spec;
		squeakPackagesAndGroups: spec
]

{ #category : #accessing }
BaselineOfFuelPlatform >> customProjectAttributes [
	^ customProjectAttributes ifNil: [ customProjectAttributes := self platformAttributes ]
]

{ #category : #accessing }
BaselineOfFuelPlatform >> loadAll [
	#(pharo 'pharo1.x' 'pharo2.x' 'pharo3.x' 'pharo4.x' 'pharo5.x' 'pharo6.x' 'pharo7.x'
		squeak 'squeak4.x' 'squeak5.x' 'squeak6.x') do: [ :attribute |
			self addCustomProjectAttribute: attribute ]
]

{ #category : #helpers }
BaselineOfFuelPlatform >> pharoPackagesAndGroups: spec [
	spec for: #pharo do: [
		spec package: 'Fuel-Platform-Pharo-Core' with: [ spec requires: 'Fuel-Platform-Core' ] ].
	spec for: #'pharo1.x' do: [
		spec package: 'Fuel-Platform-Pharo-01x' with: [ spec requires: 'Fuel-Platform-Pharo-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: 'Fuel-Platform-Pharo-01x' ].
	spec for: #'pharo2.x' do: [
		spec package: 'Fuel-Platform-Pharo-02' with: [ spec requires: 'Fuel-Platform-Pharo-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: 'Fuel-Platform-Pharo-02' ].
	spec for: #'pharo3.x' do: [
		spec package: 'Fuel-Platform-Pharo-03' with: [ spec requires: 'Fuel-Platform-Pharo-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: 'Fuel-Platform-Pharo-03' ].
	spec for: #'pharo4.x' do: [
		spec package: 'Fuel-Platform-Pharo-04' with: [ spec requires: 'Fuel-Platform-Pharo-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: 'Fuel-Platform-Pharo-04' ].
	spec for: #'pharo5.x' do: [
		spec package: 'Fuel-Platform-Pharo-05' with: [ spec requires: 'Fuel-Platform-Pharo-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: 'Fuel-Platform-Pharo-05' ].
	spec for: #'pharo6.x' do: [
		spec package: 'Fuel-Platform-Pharo-06' with: [ spec requires: 'Fuel-Platform-Pharo-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: 'Fuel-Platform-Pharo-06' ].
	spec for: #'pharo7.x' do: [
		spec package: 'Fuel-Platform-Pharo-07' with: [ spec requires: 'Fuel-Platform-Pharo-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: 'Fuel-Platform-Pharo-07' ].
	spec for: #'pharo8.x' do: [
		spec package: 'Fuel-Platform-Pharo-08' with: [ spec requires: 'Fuel-Platform-Pharo-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: #('Fuel-Platform-Pharo-08') ].
	spec for: #'pharo9.x' do: [
		spec package: 'Fuel-Platform-Pharo-09' with: [ spec requires: 'Fuel-Platform-Pharo-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: #('Fuel-Platform-Pharo-09') ].
	spec for: #'pharo10.x' do: [
		spec package: 'Fuel-Platform-Pharo-10' with: [ spec requires: 'Fuel-Platform-Pharo-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: #('Fuel-Platform-Pharo-10') ].
	spec for: #'fuel-fallback' do: [
		spec package: 'Fuel-Platform-Pharo-10' with: [ spec requires: 'Fuel-Platform-Pharo-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: #('Fuel-Platform-Pharo-10') ]
]

{ #category : #adding }
BaselineOfFuelPlatform >> platformAttributes [
	(SystemVersion current version beginsWith: 'Pharo10') ifTrue: [ ^ #('pharo10.x') ].
	(SystemVersion current version beginsWith: 'Pharo9') ifTrue: [ ^ #('pharo9.x') ].
	(SystemVersion current version beginsWith: 'Pharo8') ifTrue: [ ^ #('pharo8.x') ].
	(SystemVersion current version beginsWith: 'Pharo7') ifTrue: [ ^ #('pharo7.x') ].
	(SystemVersion current version beginsWith: 'Pharo6') ifTrue: [ ^ #('pharo6.x') ].
	(SystemVersion current version beginsWith: 'Pharo5') ifTrue: [ ^ #('pharo5.x') ].
	(SystemVersion current version beginsWith: 'Pharo4') ifTrue: [ ^ #('pharo4.x') ].
	(SystemVersion current version beginsWith: 'Pharo3') ifTrue: [ ^ #('pharo3.x') ].
	(SystemVersion current version beginsWith: 'Pharo2') ifTrue: [ ^ #('pharo2.x') ].
	(SystemVersion current version beginsWith: 'Pharo6') ifTrue: [ ^ #('pharo6.x') ].
	(SystemVersion current version beginsWith: 'Pharo1.') ifTrue: [ ^ #('pharo1.x') ].
	(#('Pharo1.1' 'Pharo-1.1') anySatisfy: [ :version |
			SystemVersion current version beginsWith: version ]) ifTrue: [ ^ #('pharo1.x') ].
	
	(SystemVersion current version beginsWith: 'Squeak6') ifTrue: [ ^ #('squeak6.x') ].
	(SystemVersion current version beginsWith: 'Squeak5') ifTrue: [ ^ #('squeak5.x') ].
	((SystemVersion current version beginsWith: 'Squeak4.6') or: [
			"See #fixSystemVersion"
			[ (Smalltalk imageName includesSubString: '4.6') ]
				on: MessageNotUnderstood
				do: [ false ] ]) ifTrue: [ ^ #('squeak4.x') ].
	(SystemVersion current version beginsWith: 'Squeak4.') ifTrue: [ ^ #('squeak4.x') ].
	
	
	Warning signal: 'Could not determine image version. Using fallback'.
	^ #('fuel-fallback')
]

{ #category : #helpers }
BaselineOfFuelPlatform >> squeakPackagesAndGroups: spec [
	spec for: #squeak do: [
		spec package: 'Fuel-Platform-Squeak-Core' with: [ spec requires: 'Fuel-Platform-Core' ] ].
	spec for: #'squeak4.x' do: [
		spec package: 'Fuel-Platform-Squeak-04x' with: [ spec requires: 'Fuel-Platform-Squeak-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: 'Fuel-Platform-Squeak-04x' ].
	spec for: #'squeak5.x' do: [
		spec package: 'Fuel-Platform-Squeak-05' with: [ spec requires: 'Fuel-Platform-Squeak-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: 'Fuel-Platform-Squeak-05' ].
	spec for: #'squeak6.x' do: [
		spec package: 'Fuel-Platform-Squeak-06' with: [ spec requires: 'Fuel-Platform-Squeak-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: 'Fuel-Platform-Squeak-06' ].
	spec for: #'fuel-fallback' do: [
		spec package: 'Fuel-Platform-Squeak-06' with: [ spec requires: 'Fuel-Platform-Squeak-Core' ].
		spec group: 'FuelPlatformCurrentGroup' with: 'Fuel-Platform-Squeak-06' ]
]
