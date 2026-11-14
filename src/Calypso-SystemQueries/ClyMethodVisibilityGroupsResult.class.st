"
I am a class query result whch represent classes as ClyInheritedMethodGroup instances wrapped as browser items.
I am used to represent classes when you expand first item in the third pane of full browser.
ClyInheritedMethodGroup instances provide tools to control method visibility in the browser by showing/hidding methods of corresponding inherited classes.

I sort classes hierarchically according to specified hierarchy. You can create my instances with: 

	ClyMethodVisibilityGroupsResult withHierarchy: ClySubclassHierarchy inverse.
	
Tools create me with default hierarchy using: 
	
	ClyMethodVisibilityGroupsResult withDefaultHierarchy.

Calypso provides settings to invert default hierarchy and to extend it with plugins. 
The default hierarchy is built by the class answered by #methodVisibilityProvider: the class whose class-side method marked with <calypsoMethodVisibilityProvider: aPriority> has the highest priority, ClyTraitUserHierarchy when the traits plugin is loaded and ClySubclassHierarchy otherwise.

Internal Representation and Key Implementation Points.

    Instance Variables
	hierarchy:		<ClyClassHierarchy>
"
Class {
	#name : 'ClyMethodVisibilityGroupsResult',
	#superclass : 'ClyBrowserQueryResult',
	#instVars : [
		'hierarchy'
	],
	#category : 'Calypso-SystemQueries-Results',
	#package : 'Calypso-SystemQueries',
	#tag : 'Results'
}

{ #category : 'instance creation' }
ClyMethodVisibilityGroupsResult class >> defaultHierarchy [

	^ self methodVisibilityProvider hierarchyForMethodVisibility
]

{ #category : 'instance creation' }
ClyMethodVisibilityGroupsResult class >> methodVisibilityProvider [
	"The class which builds the hierarchy of the method visibility groups (#hierarchyForMethodVisibility). It is the class whose class-side method marked with <calypsoMethodVisibilityProvider: aPriority> has the highest priority: ClySubclassHierarchy, or ClyTraitUserHierarchy when the traits plugin is loaded. The class annotations of the deprecated Commander-Calypso-Compatibility package (ClyMethodVisibilityProviderAnnotation) are still taken into account."

	| providers |
	providers := (Pragma allNamed: #calypsoMethodVisibilityProvider:) collect: [ :pragma | pragma arguments first -> pragma methodClass instanceSide ].
	providers := providers , (self environment
		              at: #ClyMethodVisibilityProviderAnnotation
		              ifPresent: [ :annotationClass | annotationClass registeredInstances asArray collect: [ :annotation | annotation priority -> annotation annotatedClass ] ]
		              ifAbsent: [ #(  ) ]).
	providers ifEmpty: [ self error: 'No hierarchy provider is found to represent method visibility hierarchy' ].
	^ (providers detectMax: [ :provider | provider key ]) value
]

{ #category : 'instance creation' }
ClyMethodVisibilityGroupsResult class >> withDefaultHierarchy [

	^self withHierarchy: self defaultHierarchy
]

{ #category : 'instance creation' }
ClyMethodVisibilityGroupsResult class >> withHierarchy: aClassHierarchy [

	^self new
		hierarchy: aClassHierarchy
]

{ #category : 'comparing' }
ClyMethodVisibilityGroupsResult >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^false ].

	^ hierarchy = anObject hierarchy
]

{ #category : 'building' }
ClyMethodVisibilityGroupsResult >> buildGroupFor: aClass depth: depth toShowMethodsFrom: aScope [

	| group item |
	group := ClyInheritedMethodGroup withMethodsOf: aClass toShowIn: aScope.
	item := group asCalypsoBrowserItem.
	item depth: depth.
	items add: item
]

{ #category : 'building' }
ClyMethodVisibilityGroupsResult >> fillWith: classes [
	| actualClassScope classMap addedClasses |
	actualClassScope := self scope asLocalClassScope.
	classMap := hierarchy buildFrom: classes.
	addedClasses := Set new.
	classMap doInOrderWithDepth: [ :each :depth |
		(addedClasses includes: each) ifFalse: [
			addedClasses add: each.
			self buildGroupFor: each depth: depth toShowMethodsFrom: actualClassScope]]
]

{ #category : 'comparing' }
ClyMethodVisibilityGroupsResult >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: hierarchy hash
]

{ #category : 'accessing' }
ClyMethodVisibilityGroupsResult >> hierarchy [
	^ hierarchy
]

{ #category : 'accessing' }
ClyMethodVisibilityGroupsResult >> hierarchy: anObject [
	hierarchy := anObject
]
