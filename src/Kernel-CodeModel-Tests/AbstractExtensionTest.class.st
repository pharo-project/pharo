Class {
	#name : 'AbstractExtensionTest',
	#superclass : 'TestCase',
	#instVars : [
		'environment',
		'isolatingRoot',
		'definerPackage',
		'userPackage',
		'extendedPackage'
	],
	#category : 'Kernel-CodeModel-Tests-Extensions',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Extensions'
}

{ #category : 'compiling' }
AbstractExtensionTest >> compile: aString on: aClass [

	^ aClass
		compile: aString
		classified: nil
		withStamp: DateAndTime now
		logSource: false
]

{ #category : 'running' }
AbstractExtensionTest >> createClassNamed: className superclass: superclass package: package [

	^ ((superclass << className)
		   installingEnvironment: environment;
		   package: package)
		  sourcesManager: NoSourcesManager new;
		  install
]

{ #category : 'running' }
AbstractExtensionTest >> createExtensionNamed: x forBehavior: y inPackage: z [

	^ ((Extension << x)
		   extendedClass: y;
		   installingEnvironment: environment;
		   package: z)
		  sourcesManager: NoSourcesManager new;
		  install
]

{ #category : 'running' }
AbstractExtensionTest >> setUp [

	super setUp.

	environment := SystemEnvironment new.
	environment codeChangeAnnouncer: SystemAnnouncer new.
	environment undeclaredRegistry: Dictionary new.
	
	"We create an anonymous subclass of object.
	Anonymous subclasses handle installation silently and leave less traces in the system.
	All our classes should inherit from this one (or a subclass of it)"
	isolatingRoot := Object newAnonymousSubclass.
	
	extendedPackage := Package named: 'Extended'.
	definerPackage := Package named: 'Definer'.
	userPackage := Package named: 'User'.
]

{ #category : 'running' }
AbstractExtensionTest >> tearDown [
	"Make sure we did not pollute the global dictionaries"
	
	environment allClassesAndTraits do: [ :e | e removeFromSystem: false ].
	
	super tearDown
]
