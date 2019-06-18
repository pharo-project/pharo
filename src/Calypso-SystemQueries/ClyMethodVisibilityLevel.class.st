"
I am a root of hierarchy of different kind of method visibilities.
Instances are created with currently visible class scope and extra scope which can be activated.

Activation/decactivation methods returns new scope which represent result method visibility: 

- activateExtraScope 
- deactivateExtraScope 

My instances are used by browser to switch method visibility of inherited classes. They use simple method #toggleScope which also returns result method scope.
To represent current state of visible scope I compute #isActive flag:
If any of classes from extra scope is included in the visible scope then my instance is considered active

My instances are created on currently visible class scope. And subclasses create extra scope according to their logic.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	visibleClassScope:		<ClyClassScope>
	extraClassScope:		<ClyAbstractClassScope>
	isActive:		<Boolean>
	


    Implementation Points
"
Class {
	#name : #ClyMethodVisibilityLevel,
	#superclass : #Object,
	#instVars : [
		'visibleClassScope',
		'extraClassScope',
		'isActive'
	],
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #accessing }
ClyMethodVisibilityLevel class >> allAvailableFor: items [
	
	^ self allSubclasses select: [ :each | 
		each isActiveByDefault and: (each canBeAppliedForClasses: items) ]
]

{ #category : #testing }
ClyMethodVisibilityLevel class >> canBeAppliedForClasses: classItems [
	^true
]

{ #category : #testing }
ClyMethodVisibilityLevel class >> isActiveByDefault [
	^false
]

{ #category : #'instance creation' }
ClyMethodVisibilityLevel class >> of: aClassScope [
	^self new 
		visibleClassScope: aClassScope
]

{ #category : #controlling }
ClyMethodVisibilityLevel >> activateExtraScope [
	^visibleClassScope increasedByClassesFrom: extraClassScope
]

{ #category : #controlling }
ClyMethodVisibilityLevel >> deactivateExtraScope [
	^visibleClassScope reducedByClassesFrom: extraClassScope
]

{ #category : #controlling }
ClyMethodVisibilityLevel >> detectActiveState [

	extraClassScope classesDo: [:each | 
		(visibleClassScope includesClass: each) ifTrue: [ 
			^isActive := true ] ].
	
	isActive := false
]

{ #category : #accessing }
ClyMethodVisibilityLevel >> extraClassScope [
	^ extraClassScope
]

{ #category : #accessing }
ClyMethodVisibilityLevel >> extraClassScope: aScope [
	extraClassScope := aScope.
	self detectActiveState
]

{ #category : #testing }
ClyMethodVisibilityLevel >> isActive [
	^isActive
]

{ #category : #testing }
ClyMethodVisibilityLevel >> isRedundant [
	"the level is reduntant if there are no extra classes to show"
	extraClassScope classesDo: [ :each | ^false ].
	^true
]

{ #category : #controlling }
ClyMethodVisibilityLevel >> toggleScope [
	^self isActive
		ifTrue: [ self deactivateExtraScope ]
		ifFalse: [ self activateExtraScope ]
]

{ #category : #accessing }
ClyMethodVisibilityLevel >> visibleClassScope [
	^ visibleClassScope
]

{ #category : #accessing }
ClyMethodVisibilityLevel >> visibleClassScope: aClassScope [
	"We should cut off any inherited scopes here.
	Visible class scope must be an instance of ClyClassScope"
	visibleClassScope := aClassScope asLocalClassScope
]
