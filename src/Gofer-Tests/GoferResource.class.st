"
SUnit test resource for Gofer testing
"
Class {
	#name : 'GoferResource',
	#superclass : 'TestResource',
	#instVars : [
		'versionReferences',
		'monticelloRepository'
	],
	#category : 'Gofer-Tests-Resources',
	#package : 'Gofer-Tests',
	#tag : 'Resources'
}

{ #category : 'accessing' }
GoferResource >> monticelloRepository [

	^ monticelloRepository
]

{ #category : 'running' }
GoferResource >> setUp [

	super setUp.
	self
		setUpVersionReferences;
		setUpMonticelloRepository
]

{ #category : 'private - running' }
GoferResource >> setUpMonticelloRepository [
	"This method builds a fake repository with the version references from #buildReferences."

	| packages |
	packages := Dictionary new.
	monticelloRepository := MCDictionaryRepository new.
	versionReferences do: [ :reference |
		monticelloRepository basicStoreVersion: (MCVersion new
				 setPackage: (packages at: reference packageName ifAbsentPut: [ MCPackage new name: reference packageName ])
				 info: (MCVersionInfo
						  name: reference name
						  id: UUID new
						  message: 'This is a mock version'
						  date: Date today
						  time: Time now
						  author: reference author
						  ancestors: #(  ))
				 snapshot: (MCSnapshot fromDefinitions:
						  (Array with: (MCOrganizationDefinition packageName: reference packageName) with: ((MCClassDefinition named: (reference packageName copyWithout: $-))
								    packageName: reference packageName;
								    yourself)))
				 dependencies: #(  )) ]
]

{ #category : 'private - running' }
GoferResource >> setUpVersionReferences [
	"This method answers a set of Gofer references in the order they should be sorted. It includes two different packages (Gofer-Foo, Gofer-Bar), linear series of packages (Gofer-Foo-lr.1, Gofer-Foo-lr.2, Gofer-Foo-lr.4), packages with a branch (Gofer-Bar.branch-lr.1,Gofer.branch-Bar-lr.2), and packages with the same version but different authors (Gofer-Bar-jf.1, Gofer-Bar-lr.1)."

	versionReferences := OrderedCollection new.
	versionReferences
		add: (GoferVersionReference name: 'GoferBar.branch-lr.1');
		add: (GoferVersionReference name: 'GoferBar.branch-lr.2');
		add: (GoferVersionReference name: 'GoferBar-jf.1');
		add: (GoferVersionReference name: 'GoferBar-lr.1');
		add: (GoferVersionReference name: 'GoferFoo-lr.1');
		add: (GoferVersionReference name: 'GoferFoo-lr.2');
		add: (GoferVersionReference name: 'GoferFoo-lr.4')
]

{ #category : 'accessing' }
GoferResource >> versionReferences [

	^ versionReferences
]
