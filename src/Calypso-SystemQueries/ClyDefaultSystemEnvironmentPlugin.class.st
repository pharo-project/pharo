"
I represent default plugin for navigation environment. 
I fill items with basic properties related to Smalltalk meta model.
And I provide standart method and class groups
"
Class {
	#name : #ClyDefaultSystemEnvironmentPlugin,
	#superclass : #ClySystemEnvironmentPlugin,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'item decoration' }
ClyDefaultSystemEnvironmentPlugin >> checkPackageHasClassGroups: aPackage [
	| providers packageScope |
	packageScope := ClyPackageScope of: aPackage in: environment.
	environment
		pluginsDo: [ :plugin | 
			providers := plugin collectClassGroupProviders.
			providers
				do: [ :each | each classGroupsIn: packageScope do: [ :group | ^ true ] ] ].
	^ false
]

{ #category : #'method groups' }
ClyDefaultSystemEnvironmentPlugin >> collectClassGroupProviders [

	^{ClyExtendedClassGroupProvider. ClyNoTagClassGroupProvider. ClyTaggedClassGroupProvider}
		collect: [ :each | each new]
]

{ #category : #'method groups' }
ClyDefaultSystemEnvironmentPlugin >> collectMethodGroupProviders [ 

	^{ClyInheritedMethodGroupProvider. ClyTaggedMethodGroupProvider. ClyUnclassifiedMethodGroupProvider. ClyExtendedMethodGroupProvider}
		collect: [ :each | each new]
]

{ #category : #'item decoration' }
ClyDefaultSystemEnvironmentPlugin >> decorateBrowserItem: anItem ofClass: aClass [

	anItem ensureSystemDefinition: [ClyClassDefinitionProperty of: aClass].
	
	aClass localSelectors ifNotEmpty: [ anItem markWithChildrenOf: ClyMethod].

	aClass hasComment ifFalse: [ anItem markWith: ClyNoCommentTag]
]

{ #category : #'item decoration' }
ClyDefaultSystemEnvironmentPlugin >> decorateBrowserItem: anItem ofClassGroup: aClassGroup [
	
	aClassGroup decorateOwnBrowserItem: anItem
]

{ #category : #'item decoration' }
ClyDefaultSystemEnvironmentPlugin >> decorateBrowserItem: anItem ofMethod: aMethod [
	
	anItem ensureSystemDefinition: [ClyMethodDefinitionProperty of: aMethod]
]

{ #category : #'item decoration' }
ClyDefaultSystemEnvironmentPlugin >> decorateBrowserItem: anItem ofMethodGroup: aMethodGroup [
	
	aMethodGroup decorateOwnBrowserItem: anItem
]

{ #category : #'item decoration' }
ClyDefaultSystemEnvironmentPlugin >> decorateBrowserItem: anItem ofPackage: aPackage [
	
	aPackage classes ifNotEmpty: [ anItem markWithChildrenOf: ClyClass ].
	
	(self checkPackageHasClassGroups: aPackage) ifTrue: [ 
		anItem markWithChildrenOf: ClyClassGroup]
]
