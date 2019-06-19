"
I represent visibility of all inherited methods.
I limit inheritance up to Object to avoid less interesting methods. So Object and ProtoObject methods are not shown when you activate me.

I create extra scope using #asInheritedScope message.
"
Class {
	#name : #ClyFullVisibilityLevel,
	#superclass : #ClyMethodVisibilityLevel,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'instance creation' }
ClyFullVisibilityLevel class >> of: aClassScope [
	
	^(super of: aClassScope)
		extraClassScope: aClassScope asInheritedScope
]

{ #category : #controlling }
ClyFullVisibilityLevel >> activateExtraScope [
	"We only want to show interesting classes which means we do not case about Object and rest"
	^visibleClassScope increasedByClassesFrom: extraClassScope asInterestingClassScope
]

{ #category : #testing }
ClyFullVisibilityLevel >> isRedundant [
	"the level is reduntant if there are no interesting classes to show"
	extraClassScope asInterestingClassScope classesDo: [ :each | ^false ].
	^true
]
