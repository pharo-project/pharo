"
I implement a registry of class annotations.
I organize it in two dictionaries:

1)  annotations
It maps annotation class to all instances of this class. Instances are sorted by annotation priority in ascending order. 
Annotation class is responsible to provide such container: 

	ClassAnnotation createContainerForRegistry

For details about annotation priorities look at ClassAnnotation comment.

2) annotatedClasses
It maps annotated class to all its annotations. The annotations are managed as a Set.

I provide default instance which keeps all class annotations in the system.

	ClassAnnotationRegistry default
	
 It is used by ClassAnnotation query methods and you can access it from it:
	
	ClassAnnotation registry

I am subscribed on system changes and reset default instance when related methods or classes are changed.
And when user access annotations I rebuild it lazily. 
Look for details in class side method #ensureSystemSubscription  

Internal Representation and Key Implementation Points.

    Instance Variables
	annotations:		<IdentityDictionary<ClassAnnotation class, SortedCollection<ClassAnnotation>>>
	annotatedClasses:		<IdentityDictionary<Class, Set<ClassAnnotation>>>
"
Class {
	#name : #ClassAnnotationRegistry,
	#superclass : #Object,
	#instVars : [
		'annotations',
		'annotatedClasses'
	],
	#classInstVars : [
		'default'
	],
	#category : #ClassAnnotation
}

{ #category : #'pragma collecting' }
ClassAnnotationRegistry class >> annotationDependencyPragmaName [
	"This pragma is used to mark methods which are sent inside annotation definition methods.
	They are annotation dependencies.
	For example CmdShortcutCommandActivation annotation has method #removaFor:.
	It provides reusable cmd+x shorcut. And all removal related commands are depends on it.
	When user will modify this method the annotation registry will be updated"
	
	^#classAnnotationDependency
]

{ #category : #'pragma collecting' }
ClassAnnotationRegistry class >> annotationPragmaName [
	"This pragma is used to define class annotations inside methods.
	They should return instances of ClassAnnotation subclasses"
	
	^#classAnnotation
]

{ #category : #'pragma collecting' }
ClassAnnotationRegistry class >> annotationPragmasIn: aClass do: aBlock [

	Pragma withPragmasIn: aClass do: [ :pragma |
		pragma selector == self annotationPragmaName ifTrue: [aBlock value: pragma]]
]

{ #category : #'pragma collecting' }
ClassAnnotationRegistry class >> collectAnnotationPragmas [

	| result |
	result := OrderedCollection new.
	
	self environment allClassesDo: [ :each | 
		self annotationPragmasIn: each classSide do: [:pragma | result add: pragma]].
	self environment allTraitsDo: [ :each | 
		self annotationPragmasIn: each classSide do: [:pragma | result add: pragma]].
	
	^result
]

{ #category : #'instance creation' }
ClassAnnotationRegistry class >> collectNewFromSystem [
	^self new
		addAllFromPragmas: self collectAnnotationPragmas
]

{ #category : #accessing }
ClassAnnotationRegistry class >> default [
	^default ifNil: [ 
		self ensureSystemSubscription.
		default := self collectNewFromSystem]
]

{ #category : #'system changes' }
ClassAnnotationRegistry class >> doesMethodAffectAnnotations: aMethod [
	
	(self doesMethodDefineAnnotation: aMethod) ifTrue: [ ^true ].

	aMethod methodClass superclass ifNotNil: [ :parentClass | 
		"check if there is overridden method which defines annotation"
		(parentClass lookupSelector: aMethod selector) ifNotNil: [:overriddenMethod |
			(self doesMethodDefineAnnotation: overriddenMethod) ifTrue: [^true]].
	].	

	^false
]

{ #category : #'system changes' }
ClassAnnotationRegistry class >> doesMethodDefineAnnotation: aMethod [
	"The second condition allows to mark methods 
	which are sent inside annotation definition methods 
	which makes them annotation dependency.
	For example CmdShortcutCommandActivation annotation has method #removaFor:.
	It provides reusable cmd+x shorcut. And all removal related commands are depends on it.
	When user will modify this method the annotation registry will be updated 
	because of <classAnnotationDependency> pragma"
	
	^(aMethod hasPragmaNamed: self annotationPragmaName)
		or: [ aMethod hasPragmaNamed: self annotationDependencyPragmaName ]
]

{ #category : #'system changes' }
ClassAnnotationRegistry class >> ensureSystemSubscription [

	SystemAnnouncer uniqueInstance unsubscribe: self.
	
	SystemAnnouncer uniqueInstance weak
		when: ClassRemoved, ClassAdded, ClassModificationApplied send: #handleClassChanged: to: self;
		when: MethodRemoved, MethodAdded, MethodModified send: #handleMethodChange: to: self;
		when: MethodModified send: #handleOldMethodChange: to: self
]

{ #category : #'system changes' }
ClassAnnotationRegistry class >> handleClassChanged: aClassAnnouncement [

	self resetIf: [ 
		(default includesAnnotationsFor: aClassAnnouncement classAffected)
			or: [default includesAnnotationsInheritedBy: aClassAnnouncement classAffected]]
]

{ #category : #'system changes' }
ClassAnnotationRegistry class >> handleMethodChange: aMethodAnnouncement [
	
	self resetIf: [ self doesMethodAffectAnnotations: aMethodAnnouncement methodAffected]
]

{ #category : #'system changes' }
ClassAnnotationRegistry class >> handleOldMethodChange: aMethodModified [
	
	self resetIf: [ self doesMethodAffectAnnotations: aMethodModified oldMethod]
]

{ #category : #initialization }
ClassAnnotationRegistry class >> reset [
	<script>
	
	SystemAnnouncer uniqueInstance unsubscribe: self.
	default := nil
]

{ #category : #accessing }
ClassAnnotationRegistry class >> resetIf: conditionBlock [
	default ifNil: [ ^self ].
	
	conditionBlock value ifTrue: [ self reset ]
]

{ #category : #accessing }
ClassAnnotationRegistry >> addAllFromPragmas: pragmas [

	| collectedAnnotations |
	pragmas do: [ :pragma |
		collectedAnnotations := self createAnnotationsForAllSubclassesFromPragma: pragma.
		collectedAnnotations do: [ :each | self addAnnotation: each]]
]

{ #category : #accessing }
ClassAnnotationRegistry >> addAnnotation: aClassAnnotation [
	| container actualAnnotation |
	actualAnnotation := aClassAnnotation.
	actualAnnotation isRedefined ifTrue: [
		actualAnnotation updateRedefinedInstance.
		actualAnnotation := actualAnnotation redefiningInstance].
	
	container := annotations at: actualAnnotation class ifAbsentPut: [ 
		aClassAnnotation class createContainerForRegistry ].
	(container includes: actualAnnotation) ifTrue: [ ^self ].
	container add: actualAnnotation.

	container := annotatedClasses 
		at: actualAnnotation annotatedClass ifAbsentPut: [ Set new].
	container add: actualAnnotation	
]

{ #category : #queries }
ClassAnnotationRegistry >> allAnnotating: aClass [

	^annotatedClasses at: aClass ifAbsent: [ #() ]
]

{ #category : #queries }
ClassAnnotationRegistry >> allInstancesOf: anAnnotationClass [

	^annotations at: anAnnotationClass ifAbsent: [ #() ]
]

{ #category : #accessing }
ClassAnnotationRegistry >> annotatedClasses [
	^ annotatedClasses
]

{ #category : #accessing }
ClassAnnotationRegistry >> annotatedClasses: anObject [
	annotatedClasses := anObject
]

{ #category : #accessing }
ClassAnnotationRegistry >> annotations [
	^ annotations
]

{ #category : #accessing }
ClassAnnotationRegistry >> annotations: anObject [
	annotations := anObject
]

{ #category : #private }
ClassAnnotationRegistry >> createAnnotationFor: aClass fromPragma: aPragma [
	"Here we evaluate pragma method expecting annotation instance as result.
	Of course the method can fail or it can not return annotation 
	(user can forgot return for example). 
	In such cases BrokenClassAnnotation will be used instead.
	Also here we perform check isForbidden which should be evaluated on initialized instance 
	and also can fail due to bad implementation. 
	Failed check will also lead to BrokenClassAnnotation instance.
	If annotation instance is forbidden 
	then the ForbiddenClassAnnotation instance will be returned instead.
	And it will will not be added to the registry"
	| annotation |
	[
		annotation := aClass perform: aPragma methodSelector.
		(annotation isKindOf: ClassAnnotation) ifFalse: [ 
			annotation := BrokenClassAnnotation forBadMethodReturn].
		"isForbidden should be performed with initialized instance"
		annotation 
			annotatedClass: aClass;
			declarationSelector: aPragma methodSelector.
		annotation isForbidden ifTrue: [ annotation := ForbiddenClassAnnotation new ]
	] on: Error do: [:err | 
		annotation := BrokenClassAnnotation withError: err].
	"For simplisity initialization is repeated although it can be already initialized"
	^annotation 
		annotatedClass: aClass;
		declarationSelector: aPragma methodSelector
]

{ #category : #private }
ClassAnnotationRegistry >> createAnnotationsForAllSubclassesFromPragma: aPragma [
	
	| declaringClass annotation result |
	result := OrderedCollection new.
	declaringClass := aPragma methodClass instanceSide.
	declaringClass withAllSubclassesDo: [ :annotatedClass | 
		annotation := self createAnnotationFor: annotatedClass fromPragma: aPragma. 
		result add: annotation].
	^result
]

{ #category : #accessing }
ClassAnnotationRegistry >> forgetAnnotation: aClassAnnotation [
	"This test was created for tests to emulate that annotation not exist anymore"
	| container |

	container := annotations at: aClassAnnotation class ifAbsent: [ ^self ].
	container remove: aClassAnnotation ifAbsent: [ ^self ].
	
	(annotatedClasses at: aClassAnnotation annotatedClass) remove: aClassAnnotation
]

{ #category : #testing }
ClassAnnotationRegistry >> includesAnnotation: aClassAnnotation [

	| allClassAnnotations |
	allClassAnnotations := annotatedClasses at: aClassAnnotation annotatedClass ifAbsent: [ ^false ].
	^allClassAnnotations includes: aClassAnnotation
]

{ #category : #testing }
ClassAnnotationRegistry >> includesAnnotationsFor: aClass [

	^annotatedClasses includesKey: aClass
]

{ #category : #testing }
ClassAnnotationRegistry >> includesAnnotationsInheritedBy: aClass [

	aClass superclass ifNil: [ ^false ].
	
	^self includesAnnotationsFor: aClass superclass
]

{ #category : #initialization }
ClassAnnotationRegistry >> initialize [
	super initialize.
	annotations := IdentityDictionary new.
	annotatedClasses := IdentityDictionary new
]
