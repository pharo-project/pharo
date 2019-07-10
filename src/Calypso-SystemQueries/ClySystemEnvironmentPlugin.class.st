"
I am a root of ClySystemEnvironment navigation plugins hierarchy.
My subclasses extend system objects and their properties.

For example there is ClySUnitEnvironmentPlugin. It adds test related properties to methods, classes and packages. Also it provides new kind of method group to represent broken tests.

To extend objects subclasses should implement methods: 
	- resolvePropertiesOfClass: classItem
	- resolvePropertiesOfMethod: methodItem
	- look at ""items resolving"" protocol for more

Plugins can provide new kind of package, class and method groups. They return own group providers in following methods:
	- collectMethodGroupProviders
	- collectClassGroupProviders

Notice that current image environment adds all plugins automatically. 
Look at superclass ClyEnvironmentPlugin for responsibility details.

For subclasses I provide notion of slow plugins and methods to enable/disable them:

- disableSlowPlugins
- enableSlowPlugins

Any system plugin can implement #isSlow as true. It is important for slow machines (like Raspberry) to be able easily disable all heavy logic in the browser.
"
Class {
	#name : #ClySystemEnvironmentPlugin,
	#superclass : #ClyEnvironmentPlugin,
	#classVars : [
		'SlowPluginsAreDisabled'
	],
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #testing }
ClySystemEnvironmentPlugin class >> disableSlowPlugins [
	<script>
	SlowPluginsAreDisabled := true
]

{ #category : #testing }
ClySystemEnvironmentPlugin class >> enableSlowPlugins [
	<script>
	SlowPluginsAreDisabled := false
]

{ #category : #testing }
ClySystemEnvironmentPlugin class >> isAbstract [
	^self = ClySystemEnvironmentPlugin 
]

{ #category : #testing }
ClySystemEnvironmentPlugin class >> isAutoActivated [
	(SlowPluginsAreDisabled = true and: [ self isSlow ]) ifTrue: [ ^false ].
	
	^super isAutoActivated
]

{ #category : #testing }
ClySystemEnvironmentPlugin class >> isRelatedTo: aSystem [

	^aSystem isKindOf: ClySystemEnvironment
]

{ #category : #testing }
ClySystemEnvironmentPlugin class >> isSlow [
	^false
]

{ #category : #'groups collecting' }
ClySystemEnvironmentPlugin >> collectClassGroupProviders [
	^#()
]

{ #category : #'query metadata' }
ClySystemEnvironmentPlugin >> collectMetadataOfClassGroups: aQueryResult [
]

{ #category : #'query metadata' }
ClySystemEnvironmentPlugin >> collectMetadataOfClasses: aQueryResult [
]

{ #category : #'query metadata' }
ClySystemEnvironmentPlugin >> collectMetadataOfMethodGroups: aQueryResult [
]

{ #category : #'query metadata' }
ClySystemEnvironmentPlugin >> collectMetadataOfMethods: aQueryResult [
]

{ #category : #'query metadata' }
ClySystemEnvironmentPlugin >> collectMetadataOfPackages: aQueryResult [
]

{ #category : #'query metadata' }
ClySystemEnvironmentPlugin >> collectMetadataOfProjects: aQueryResult [
]

{ #category : #'groups collecting' }
ClySystemEnvironmentPlugin >> collectMethodGroupProviders [
	^#()
]

{ #category : #'item decoration' }
ClySystemEnvironmentPlugin >> decorateBrowserItem: anItem ofClass: aClass [
]

{ #category : #'item decoration' }
ClySystemEnvironmentPlugin >> decorateBrowserItem: anItem ofClassGroup: aClassGroup [
]

{ #category : #'item decoration' }
ClySystemEnvironmentPlugin >> decorateBrowserItem: anEnvironmentItem ofMethod: aMethod [

	
]

{ #category : #'item decoration' }
ClySystemEnvironmentPlugin >> decorateBrowserItem: anItem ofMethodGroup: aMethodGroup [
]

{ #category : #'item decoration' }
ClySystemEnvironmentPlugin >> decorateBrowserItem: anItem ofPackage: aPackage [
]

{ #category : #'item decoration' }
ClySystemEnvironmentPlugin >> decorateBrowserItem: anItem ofProject: aProject [ 
]
