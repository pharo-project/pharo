"
I'm a visitor to create the correct presenter to edit a scope
"
Class {
	#name : #RBEnvironmentVisitor,
	#superclass : #Object,
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #accessing }
RBEnvironmentVisitor >> chooserClass [
	^ SpChooserPresenter
]

{ #category : #visiting }
RBEnvironmentVisitor >> visitClassEnv: rbClassEnv [
	| list selectedList methodName items dialog |
	list := RBBrowserEnvironment default allClasses sorted: #name ascending.
	selectedList := (rbClassEnv classes collect:
		[ :e | Smalltalk globals at: e ifAbsent: [ nil ] ]) asArray sorted: #name ascending.
	methodName := #name.
	items := OrderedCollection new.
	dialog := self chooserClass openOn:
		{ list . selectedList . methodName }
		onAccept: [:e| items addAll: e ].
	dialog isCancelled ifTrue: [ ^self ].
	rbClassEnv classesDo: [ :cls | rbClassEnv removeClass: cls ].
	items do: [ :cls | rbClassEnv addClass: cls ]
]

{ #category : #visiting }
RBEnvironmentVisitor >> visitPackageEnv: rbPackageEnv [
	| list selectedList methodName items dialog |
	list := RPackageOrganizer default packages sorted: #packageName ascending.
	selectedList := rbPackageEnv packages asArray sorted: #packageName ascending.
	methodName := #packageName.
	items := OrderedCollection new.
	dialog := self chooserClass openOn:
		{ list . selectedList . methodName }
		onAccept: [:e| items addAll: e ].
	dialog isCancelled ifTrue: [ ^self ].

	rbPackageEnv packages do: [ :cls | rbPackageEnv removePackage: cls ].
	items do: [ :cls | rbPackageEnv addPackage: cls ]
]

{ #category : #visiting }
RBEnvironmentVisitor >> visitProtocolEnv: rbProtocolEnv [
	| list selectedList methodName items dialog |
	list := rbProtocolEnv definedClass protocolNames sorted.
	selectedList := rbProtocolEnv protocols sorted.
	methodName := #yourself.
	items := OrderedCollection new.
	dialog := self chooserClass openOn:
		{ list . selectedList . methodName }
		onAccept: [:e| items addAll: e ].
	dialog isCancelled ifTrue: [ ^self ].

	rbProtocolEnv protocols do: [ :cls | rbProtocolEnv removeProtocol: cls ].
	items do: [ :cls | rbProtocolEnv addProtocol: cls ]
]
