"
I am the root of class annotation hierarchy.
My subclasses should annotate classes using class side methods with the pragma #classAnnotation. 
For example:

	MyClass class>>specialAnnotationExample
		<classAnnotation>
		^MySpecialAnnotation new

The annotating method should return an instance of the annotation.

I provide a query API to retrieve all registered instances of a concrete annotation class:

	MySpecialAnnotation registeredInstances.
	MySpecialAnnotation registeredInstancesFor: MyClass.
	MySpecialAnnotation registeredInstancesDo: [:each | each logCr].

Each annotation includes the annotated class and the selector of declaration method.
All annotations are cached in default ClassAnnotationRegistry instance. It is cheap to query them.

Classes itself can be queried for all attached annotations:

	MyClass classAnnotations.
	MyClass classAnnotationsDo: [:each | each logCr].

I provide extra hook to forbid annotating of particular classes. For example my subclasses can define that abstract classses should not be annotated by them.
The rule should be  implemented in the method:

	MySpecialAnnotation >>isForbidden
		^annotatedClass isAbstract 

By default method returns true which means that annotation can annotate any class.

Because annotations are declared in the methods it provides interesting feature to extend meta information from external packages.
Just define declaration method as class extension. And when your package will be loaded the new annotation will be added into existing class.
 
There is no special way how instantiate annotation instances. It is up to your domain.
The internal state is initialized during Registry creation.  Users should not think about it. 
You can add any domain specific variables to your annotations and add constructors to initialize them in declaration methods. 
Annotations are just normal objects without any restrictions. You can also attatch annotations to annotations like in other languages.

-----------Advanced features. Priority------------

Annotations in the registry are sorted by priority. It provides out of the box mechanizm for ordering annotated classes for the user domain purposes. 
Just pass priority number into the annotation when you create it in declaration method. And define suitable constructor for this.

-----------Advanced features. Context------------

Any annotation can be contextual. You can specify instance of context where annotation can be used:
	MySpecialAnnotation for: anAnotationContext
Context describes annotation users where they should be active.

For simplicity you can specify any class instead of context instance. It will represent all users of annotation of particular class hierarchy:

	MySpecialAnnotation for: MyUserClass.

Internallly argument is always converted to the context:

	MyUserClass asAnnotationContext.

I provide query interface to retriev registered annotations which are active in given context:

	MySpecialAnnotation activeInstancesInContext: anAnnotationUser.
	MySpecialAnnotation activeInstancesInContext: anAnnotationUser do: [:ann | ].
	MySpecialAnnotation activeInstancesFor: MyClass inContext: anAnnotationUser do: [:ann | ].

By default the annotation is active if given user is described by declared context:

	ClassAnnotation>>isActiveInContext: anAnnotationUser
		^activeContext describes: anAnnotationUser

Subclasses can provide extra conditions for active annotations. In that case they override this method:

	MySpecialAnnotation>>isActiveInContext: anAnnotationUser
		^(super isActiveInContext: anAnnotationUser)
			and: [annotatingClass canBeUsedInContext: anAnnotationUser]

So the logic can depends on annotating class itself and actual annotation user.

For some scenarios you may need to query annotations according to original ""active"" definition despite of extra conditions.
For such cases I introduced the ""visibility"" of annotations: the annotation is visible if it is declared for given user:

	ClassAnnotation>>isVisibleInContext: anAnnotationUser
		^activeContext describes: anAnnotationUser

So the visible annotation is not necessary active. But active annotation is always visible for given user:

	ClassAnnotation>>isActiveInContext: anAnnotationUser
		^self isVisibleInContext: anAnnotationUser

(I showed another version above to simplify description).
There are extra query methods to retrieve visible annotations:

	MySpecialAnnotation visibleInstancesInContext: anAnnotationUser.
	MySpecialAnnotation visibleInstancesInContext: anAnnotationUser do: [:ann | ].
	MySpecialAnnotation visibleInstancesFor: MyClass inContext: anAnnotationUser do: [:ann | ].

-----------Advanced features. Annotation dependency methods------------

It is possible to call other methods inside annotation declaring methods (with pragma <classAnnotation>).
Such methods are dependency methods and their modification requires updating annotation cache (registry).
You can do it manually using

	ClassAnnotation resetAll
	
Or you can mark such methods with special pragma <classAnnotationDependency> and systen will track these methods for automatically.	
For example in Commander package there is CmdShortcutCommandActivation annotation. It provides reusable methods for rename and remove shorcuts: cmd+r and cmd+x. So you can annotate commands using: 

	MyRenameCommand class>>shortcutActivation
		<classAnnotation>
		^CmdShortcutCommandActivation renamingFor: MyApp.

This annotation will keep cmd+r in instance variable. 
If you will modify #renamingFor: method with new shorctut the annotations should be updated. And special pragma ensures this logic:

	CmdShortcutCommandActivation class>>renamingFor: anAnnotationUser
		<classAnnotationDependency>
		^self by: $r meta for: anAnnotationUser 

-----------Advanced features. Redefining registered instances------------

All annotations are collected from methods and cached in default ClassAnnotationRegistry instance. 
I provide special mechanizm to redefine collected instances. When cache is updated I use and keep all redefined annotations.

To redefine particular annotation use #redefineBy: message with block which sets custom properties to original instance.
For example following code allows to redefine shortcut of #browse command in Calypso: 

	(ClySpawnFullBrowserCommand classAnnotationsAt: #browserShortcutActivation)
		redefineBy: [:shortcut | shortcut keyCombination: $o meta ].
	
Try evaluate it and press cmd+o on selected item in browser. If will open new browser window.
You can notice that old shortcut cmd+b is not working anymore.

Now you can manualy reset annotation cache to check that it will not affect redefined shortcut: 

	ClassAnnotation resetCache.
	
To inspect redefined annotations ask their class for: 

	CmdShortcutCommandActivation redefinedInstances
	
Redefined instances are stored in class side variable #redefinedInstances. 
It is a dictionary which keys are new redefining annotations and values are original annotations collected from methods.
Notice that key and value are equal objects because annotations define equality using annotated class and declaration selector.
So dictionary items can be accessed using both objects.

To check that annotation is redefined use following example: 

	 (ClySpawnFullBrowserCommand classAnnotationsAt: #browserShortcutActivation)
			isRedefined

And you can ask actual redefined annotaion: 

	 (ClySpawnFullBrowserCommand classAnnotationsAt: #browserShortcutActivation)
			redefinedInstance
		
Using annotation instance you can also retrieve redefining instance: 

	anAnnotation redefiningInstance.
	
It should be identical to cached one.
	
To revert redefined annotation use #revertRedefinedInstance message: 

	(ClySpawnFullBrowserCommand classAnnotationsAt: #browserShortcutActivation)
			revertRedefinedInstance

Check that now browse command is again activated by cmd+b shortcut (which is defined in annotation declaration method).

To revert all annotations use following script: 

	CmdShortcutCommandActivation revertRedefinedInstances

Redefining logic is very suitable mehanizm to override system behavior which depends on annotations without changing the code.
It can be used to manage particular kind of annotation in settings browser. 
For example shortcut annotations based on Commander are available in setting browser. Users can explore and edit all shortcuts in the system. And these settings are persistable.

-----------Internal Representation and Key Implementation Points------------
 
    Instance Variables
	annotatedClass:		<Class>
	declarationSelector:		<Symbol>
	priority:		<Number>
	activeContext:		<AnnotationContext>
"
Class {
	#name : #ClassAnnotation,
	#superclass : #Object,
	#instVars : [
		'annotatedClass',
		'declarationSelector',
		'priority',
		'activeContext'
	],
	#classInstVars : [
		'redefinedInstances'
	],
	#category : #ClassAnnotation
}

{ #category : #'queries-contextual' }
ClassAnnotation class >> activeInstancesFor: aClass inContext: anAnnotationUser do: aBlock [
	"it will enumerate all my registered instances active for given anAnnotationUser 
	which are annotating given aClass"
	
	self registeredInstances 
		select: [ :each | 
			(each annotates: aClass) and: [each isActiveInContext: anAnnotationUser] ]
		thenDo: aBlock
]

{ #category : #'queries-contextual' }
ClassAnnotation class >> activeInstancesInContext: anAnnotationUser [
	"it will return all my registered instances which should be active for given anAnnotationUser"
	
	^self registeredInstances 
		select: [ :each | each isActiveInContext: anAnnotationUser ]
]

{ #category : #'queries-contextual' }
ClassAnnotation class >> activeInstancesInContext: anAnnotationUser do: aBlock [
	"it will enumerate all my registered instances which should be active for given anAnnotationUser"
	
	self registeredInstances 
		select: [ :each | each isActiveInContext: anAnnotationUser ]
		thenDo: aBlock
]

{ #category : #redefining }
ClassAnnotation class >> cleanRedefinedGarbage [
	"We should remove here all obsolete annotations which not exist in cache anymore.
	It can happen for several reasons: 
		- annotation method was removed.
		- annotated class was removed
		- and various changes related to class hierarchy"	
	redefinedInstances ifNil: [ ^self].
	
	(redefinedInstances select: [ :each | each isObsolete ])
		do: [ :each | redefinedInstances removeKey: each ]
]

{ #category : #private }
ClassAnnotation class >> createContainerForRegistry [
	^SortedCollection sortBlock: #priority descending
]

{ #category : #'queries-general' }
ClassAnnotation class >> findAllAnnotationsFor: aClass [ 
	
	^self registry allAnnotating: aClass
]

{ #category : #'instance creation' }
ClassAnnotation class >> for: anAnnotationContextOrCompatible [
	"For example argument can be a class which represents the context"
	^ self new 
		activeContext: anAnnotationContextOrCompatible asAnnotationContext
]

{ #category : #'for tests' }
ClassAnnotation class >> for: aContext withAnnotatedClass: aClass [
	"This method is created to be used in tests. 
	Normally annotatedClass is initialized by system during annotation registry building"
	
	^(self for: aContext) 
		annotatedClass: aClass
]

{ #category : #testing }
ClassAnnotation class >> isInstanceRedefined: aClassAnnotation [

	redefinedInstances ifNil: [ ^false ].
	
	^redefinedInstances includesKey: aClassAnnotation	
]

{ #category : #testing }
ClassAnnotation class >> isInstanceRegistered: aClassAnnotation [

	^self registry includesAnnotation: aClassAnnotation
]

{ #category : #redefining }
ClassAnnotation class >> redefineInstance: aClassAnnotation by: aBlock [
	| redefined |
	redefinedInstances ifNil: [ redefinedInstances := Dictionary new ].
	
	redefined := redefinedInstances 
		removeKey: aClassAnnotation ifAbsent: [ aClassAnnotation asRedefinedInstance ].
	redefinedInstances at: aClassAnnotation put: redefined.
	aBlock cull: aClassAnnotation
]

{ #category : #redefining }
ClassAnnotation class >> redefinedInstanceFor: aClassAnnotation [
	"It returnes the annotation instance which is redefined by given annotation"	
	redefinedInstances ifNil: [ ^self error: 'Given annotation is not redefined!' ].
	
	^redefinedInstances 
		at: aClassAnnotation ifAbsent: [ self error: 'Given annotation is not redefined!' ]
]

{ #category : #redefining }
ClassAnnotation class >> redefinedInstances [
	self cleanRedefinedGarbage.
	
	^redefinedInstances ifNil: [ #() ]
]

{ #category : #redefining }
ClassAnnotation class >> redefiningInstanceFor: aClassAnnotation [
	"It returnes the annotation instance which redefines given default collected annotation"
	
	| assoc |
	redefinedInstances ifNil: [ ^self error: 'Given annotation is not redefined!' ].
	
	assoc := redefinedInstances 
		associationAt: aClassAnnotation ifAbsent: [ self error: 'Given annotation is not redefined!' ].
	^assoc key
]

{ #category : #redefining }
ClassAnnotation class >> redefiningInstances [
	redefinedInstances ifNil: [ ^#() ].
	
	^self redefinedInstances keys
]

{ #category : #'queries-general' }
ClassAnnotation class >> registeredInstances [

	^self registry allInstancesOf: self
]

{ #category : #'queries-general' }
ClassAnnotation class >> registeredInstancesDo: aBlock [
	
	self registeredInstances do: aBlock
]

{ #category : #'queries-general' }
ClassAnnotation class >> registeredInstancesFor: aClass [ 
	"it will return all my registered instances which are annotating given aClass"
	
	^self registeredInstances
		select: [ :each | each annotates: aClass ]
]

{ #category : #private }
ClassAnnotation class >> registry [

	^ClassAnnotationRegistry default
]

{ #category : #private }
ClassAnnotation class >> resetCache [
	<script>
	ClassAnnotationRegistry reset 
]

{ #category : #redefining }
ClassAnnotation class >> revertRedefinedInstanceFor: aClassAnnotation [
	redefinedInstances ifNil: [ ^self].
	
	redefinedInstances removeKey: aClassAnnotation ifAbsent: [  ].
	self resetCache
]

{ #category : #redefining }
ClassAnnotation class >> revertRedefinedInstances [
	
	redefinedInstances := nil.
	self resetCache
]

{ #category : #redefining }
ClassAnnotation class >> updateRedefinedInstance: aClassAnnotation [
	"It updates actual redefined instance of annotation.
	It sets given annotation as new value.
	Both redefined and redefining annotations are equal. 
	So dictionary value can be used as a key"
	
	redefinedInstances ifNil: [ ^self error: 'Given annotation is not redefined!' ].
	
	redefinedInstances 
		at: aClassAnnotation ifAbsent: [ self error: 'Given annotation is not redefined!' ].
	
	redefinedInstances at: aClassAnnotation put: aClassAnnotation
]

{ #category : #'queries-contextual' }
ClassAnnotation class >> visibleInstancesFor: aClass inContext: anAnnotationUser do: aBlock [
	"it will enumerate all my registered instances visible for given anAnnotationUser 
	which are annotating given aClass"
	
	self registeredInstances 
		select: [ :each | 
			(each annotates: aClass) and: [each isVisibleInContext: anAnnotationUser] ]
		thenDo: aBlock
]

{ #category : #'queries-contextual' }
ClassAnnotation class >> visibleInstancesInContext: anAnnotationUser [
	"it will return all my registered instances which should be visible for given anAnnotationUser"
	
	^self registeredInstances 
		select: [ :each | each isVisibleInContext: anAnnotationUser ]
]

{ #category : #'queries-contextual' }
ClassAnnotation class >> visibleInstancesInContext: anAnnotationUser do: aBlock [
	"it will enumerate all my registered instances which should be visible for given anAnnotationUser"
	
	self registeredInstances 
		select: [ :each | each isVisibleInContext: anAnnotationUser ]
		thenDo: aBlock
]

{ #category : #'for tests' }
ClassAnnotation class >> withAnnotatedClass: aClass [
	"This method is created to be used in tests. 
	Normally annotatedClass is initialized by system during annotation registry building"
	
	^self new 
		annotatedClass: aClass
]

{ #category : #comparing }
ClassAnnotation >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ annotatedClass = anObject annotatedClass
		and: [ declarationSelector = anObject declarationSelector ]
]

{ #category : #accessing }
ClassAnnotation >> activeContext [
	^ activeContext
]

{ #category : #accessing }
ClassAnnotation >> activeContext: aContextDefinition [
	activeContext := aContextDefinition
]

{ #category : #accessing }
ClassAnnotation >> annotatedClass [
	^ annotatedClass
]

{ #category : #accessing }
ClassAnnotation >> annotatedClass: anObject [
	annotatedClass := anObject
]

{ #category : #testing }
ClassAnnotation >> annotates: aClass [ 
	^annotatedClass == aClass
]

{ #category : #redefining }
ClassAnnotation >> asRedefinedInstance [
	"it is a hook method to allow subclasses copy extra state if needed 
	to keep redefined instance in safe"
	^self copy
]

{ #category : #accessing }
ClassAnnotation >> declarationMethod [
	^annotatedClass classSide lookupSelector: declarationSelector
]

{ #category : #accessing }
ClassAnnotation >> declarationSelector [
	^ declarationSelector
]

{ #category : #accessing }
ClassAnnotation >> declarationSelector: anObject [
	declarationSelector := anObject
]

{ #category : #accessing }
ClassAnnotation >> freshInstance [
	"Returnes new annotation instance by evaluating declaration method.
	So new fresh instance will be return which can be different then current one"
	
	^annotatedClass perform: declarationSelector
]

{ #category : #comparing }
ClassAnnotation >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ annotatedClass hash bitXor: declarationSelector hash
]

{ #category : #accessing }
ClassAnnotation >> id [
	^annotatedClass name, '>>', declarationSelector
]

{ #category : #initialization }
ClassAnnotation >> initialize [
	super initialize.
	
	priority := 0.
	activeContext := NullAnnotationContext instance
]

{ #category : #testing }
ClassAnnotation >> isActiveInContext: anAnnotationUser [
	"The annotation can be active only when it is visible for given user.
	But other conditions can be required for concrete type of annotation.
	For example the activeness of the annotation can depends on the state of given user.
	In that case annotation should override this method with extra logic.
	But by default visible annotation is also active"
	
	^self isVisibleInContext: anAnnotationUser
]

{ #category : #testing }
ClassAnnotation >> isForbidden [
	"Subclasses can forbid annotating of particular classes.
	For exampe abstract classes can be forbidden.
	By default annotation can annotate any class"
	
	^false
]

{ #category : #testing }
ClassAnnotation >> isObsolete [

	^(self class isInstanceRegistered: self) not
]

{ #category : #testing }
ClassAnnotation >> isRedefined [
	^self class isInstanceRedefined: self
]

{ #category : #testing }
ClassAnnotation >> isSimilarTo: anotherClassAnnotation [

	^self class = anotherClassAnnotation class 
		and: [annotatedClass = anotherClassAnnotation annotatedClass
			and: [activeContext = anotherClassAnnotation activeContext]]
]

{ #category : #testing }
ClassAnnotation >> isVisibleInContext: anAnnotationUser [
	"Annotation is only visible for users which are described by activeContext"
	
	^activeContext describes: anAnnotationUser
]

{ #category : #printing }
ClassAnnotation >> printDeclarationOn: aStream [

	aStream 
		nextPut: $(;
		nextPutAll: annotatedClass name;
		nextPutAll: ' class>>';
		nextPutAll: declarationSelector;
		nextPut: $)
]

{ #category : #accessing }
ClassAnnotation >> priority [
	^ priority
]

{ #category : #accessing }
ClassAnnotation >> priority: anObject [
	priority := anObject
]

{ #category : #redefining }
ClassAnnotation >> redefineBy: modifierBlock [

	^self class redefineInstance: self by: modifierBlock
]

{ #category : #redefining }
ClassAnnotation >> redefinedInstance [

	^self class redefinedInstanceFor: self
]

{ #category : #redefining }
ClassAnnotation >> redefiningInstance [

	^self class redefiningInstanceFor: self
]

{ #category : #redefining }
ClassAnnotation >> revertRedefinedInstance [

	^self revertRedefinedInstanceIfAbsent: [
		self error: 'Redefined annotation not exists anymore!']
]

{ #category : #redefining }
ClassAnnotation >> revertRedefinedInstanceIfAbsent: absentBlock [

	self class revertRedefinedInstanceFor: self.
	^annotatedClass classAnnotations 
		detect: [ :each | each = self ] ifNone: absentBlock.
]

{ #category : #redefining }
ClassAnnotation >> updateRedefinedInstance [

	self class updateRedefinedInstance: self
]
