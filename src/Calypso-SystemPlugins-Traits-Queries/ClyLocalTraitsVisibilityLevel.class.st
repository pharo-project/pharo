"
I represent visibility of methods which are defined by all local inherited traits.
I allow to enable/disable traits visibility in full browser.

I create extra scope using:
	
	aScope asScope: ClyInheritedTraitScope
"
Class {
	#name : #ClyLocalTraitsVisibilityLevel,
	#superclass : #ClyMethodVisibilityLevel,
	#classInstVars : [
		'isDefault'
	],
	#category : #'Calypso-SystemPlugins-Traits-Queries'
}

{ #category : #testing }
ClyLocalTraitsVisibilityLevel class >> canBeAppliedForClasses: classItems [

	^classItems anySatisfy: [ :each | each isMarkedWith: ClyTraitUserTag ]
]

{ #category : #controlling }
ClyLocalTraitsVisibilityLevel class >> disable [
	isDefault := false
]

{ #category : #controlling }
ClyLocalTraitsVisibilityLevel class >> enable [
	isDefault := true
]

{ #category : #controlling }
ClyLocalTraitsVisibilityLevel class >> extendClassScope: aClassScope [
	
	| traitScope |
	traitScope := aClassScope asScope: ClyInheritedTraitScope.
	^aClassScope increasedByClassesFrom: traitScope
]

{ #category : #testing }
ClyLocalTraitsVisibilityLevel class >> isDefault [
	^isDefault ifNil: [ true ]
]

{ #category : #testing }
ClyLocalTraitsVisibilityLevel class >> isDefault: aBool [
	isDefault := aBool
]

{ #category : #'instance creation' }
ClyLocalTraitsVisibilityLevel class >> of: aClassScope [
	
	^(super of: aClassScope)
		extraClassScope: (aClassScope asScope: ClyInheritedTraitScope)
]

{ #category : #settings }
ClyLocalTraitsVisibilityLevel class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #isDefault)
		parent: #Calypso;
		label: 'Show inherited trait methods by default?';
		description: 'If false, trait methods will be hidden as superclass methods';
		target: self
		
]

{ #category : #controlling }
ClyLocalTraitsVisibilityLevel >> activateExtraScope [
	self class enable.
	
	^super activateExtraScope
]

{ #category : #'*Calypso-SystemPlugins-Traits-Queries' }
ClyLocalTraitsVisibilityLevel >> asTraitVisibility [
	^self
]

{ #category : #controlling }
ClyLocalTraitsVisibilityLevel >> deactivateExtraScope [
	self class disable.
	
	^super deactivateExtraScope
]
