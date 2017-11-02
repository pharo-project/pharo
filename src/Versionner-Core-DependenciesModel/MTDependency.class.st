"
A MTDependency is an abstraction for project dependencies.

Instance Variables
	- name : The dependency name
	- repository : The repository URL used to find this dependency
	- version : The specific version describes by the dependency
	- platforms : If not nil, the dependency is only applicable to specified platforms
	- dependencies : A dependency could have dependencies
	- project : The project root node
	- parent: The owner of the dependency.
"
Class {
	#name : #MTDependency,
	#superclass : #Object,
	#instVars : [
		'name',
		'version',
		'platforms',
		'dependencies',
		'parent'
	],
	#category : 'Versionner-Core-DependenciesModel'
}

{ #category : #instantiation }
MTDependency class >> newNamed: aName [
	"Create a new dependency with the given name."

	^ self new name: aName.
]

{ #category : #comparing }
MTDependency >> <= aMTDependency [
	^ self name <= aMTDependency name
]

{ #category : #comparing }
MTDependency >> = other [
	(self class = other class) ifFalse: [ ^ false ].
	(self name = other name) ifFalse: [ ^ false ].
	(self version = other version) ifFalse: [ ^ false ].
	(self parent = other parent) ifFalse: [ ^ false ].
	"(self platforms  = other platforms) ifFalse: [ ^false ]. "
	^ true
]

{ #category : #visiting }
MTDependency >> acceptVisitor: aMTProjectVisitor [ 
	self subclassResponsibility
]

{ #category : #protocol }
MTDependency >> addDependency: aDependency [
	(dependencies includes: aDependency) 
		ifFalse: [ [ aDependency parent: self ] on: MessageNotUnderstood do: [ "ignore" ].
				 dependencies add: aDependency ] 
]

{ #category : #displaying }
MTDependency >> asString [
	"Pretty print to display a dependency."
	^ self displayString.
]

{ #category : #accessing }
MTDependency >> dependencies [
	^ dependencies
]

{ #category : #accessing }
MTDependency >> dependencies: aCollection [
	dependencies := aCollection.
]

{ #category : #accessing }
MTDependency >> displayName [
	^ self name
]

{ #category : #displaying }
MTDependency >> displayString [
	"Pretty print to display a dependency."
	self subclassResponsibility .
]

{ #category : #initialize-release }
MTDependency >> initialize [ 
	dependencies  := OrderedCollection new.

]

{ #category : #accessing }
MTDependency >> name [
	^ name
]

{ #category : #accessing }
MTDependency >> name: anObject [
	name := anObject
]

{ #category : #accessing }
MTDependency >> parent [
	^ parent
]

{ #category : #accessing }
MTDependency >> parent: anObject [
	parent := anObject
]

{ #category : #printing }
MTDependency >> printOn: aStream [
	"Pretty print for dependencies"
	aStream nextPutAll: self name.
]

{ #category : #accessing }
MTDependency >> project [
	| myparent |
	myparent := self parent.
	[ myparent isNotNil ] whileTrue: [ [ myparent := myparent parent ] on: MessageNotUnderstood do: [ ^ myparent ] ].
	^ myparent
]

{ #category : #protocol }
MTDependency >> remove [
	"search if I am present in other groups and remove myself from these groups, 
	then remove myself from project dependencies"
	(self project groups select: [ :aGroup | aGroup dependencies includes: name ])
		do: [ :aGroup | aGroup removeDependency: name].
	self project dependencies: (self project dependencies reject: [ :dep | dep = self]).
	self project announcer announce: (MTProjectModelChanged project: self)
]

{ #category : #protocol }
MTDependency >> removeDependency: aDependency [
	dependencies remove: aDependency ifAbsent: [  ]
]

{ #category : #accessing }
MTDependency >> version [
	"Get the specific version used for this dependency"
	
	^version.
]

{ #category : #accessing }
MTDependency >> version: aVersion [
	"Set the specific version used for this dependency"
	
	version := aVersion.
]
