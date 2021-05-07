Class {
	#name : #RBEnvironmentVisitor,
	#superclass : #Object,
	#classVars : [
		'ScopeList'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #visiting }
RBEnvironmentVisitor >> visitClassEnv: rbClassEnv [
	| list selectedList methodName items dialog |
	list := RBBrowserEnvironment default allClasses sorted: #name ascending.
	selectedList := (rbClassEnv classes collect: 
		[ :e | Smalltalk globals at: e ifAbsent: [ nil ] ]) asArray sorted: #name ascending.
	methodName := #name.
	items := OrderedCollection new.
	dialog := SpChooserPresenter openOn: 
		{ list . selectedList . methodName }
		onAccept: [:e| items addAll: e ].
	dialog isCancelled ifTrue: [ ^self ].
	rbClassEnv classesDo: [ :cls | rbClassEnv removeClass: cls ].
	items do: [ :cls | rbClassEnv addClass: cls ].
]

{ #category : #visiting }
RBEnvironmentVisitor >> visitPackageEnv: rbPackageEnv [
	| list selectedList methodName items dialog |
	list := RPackageOrganizer default packages sorted: #packageName ascending.
	selectedList := rbPackageEnv packages asArray sorted: #packageName ascending.
	methodName := #packageName.
	items := OrderedCollection new.
	dialog := SpChooserPresenter openOn: 
		{ list . selectedList . methodName }
		onAccept: [:e| items addAll: e ].
	dialog isCancelled ifTrue: [ ^self ].
	
	rbPackageEnv classesDo: [ :cls | rbPackageEnv removePackage: cls ].
	items do: [ :cls | rbPackageEnv addPackage: cls ].
]

{ #category : #visiting }
RBEnvironmentVisitor >> visitProtocolEnv: rbProtocolEnv [
]
