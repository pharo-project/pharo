Class {
	#name : #MetacelloProjectRefTutorialConfig,
	#superclass : #Object,
	#instVars : [
		'project'
	],
	#category : 'Metacello-Tutorial'
}

{ #category : #private }
MetacelloProjectRefTutorialConfig class >> bootstrapPackage: aString from: aPath [ 
	| repository version |
	repository := MCHttpRepository
				location: aPath
				user: ''
				password: ''.
	repository
		versionReaderForFileNamed: aString , '.mcz'
		do: [:reader | 
			version := reader version.
			version load.
			version workingCopy repositoryGroup addRepository: repository]
]

{ #category : #private }
MetacelloProjectRefTutorialConfig class >> ensureMetacello [
	"Bootstrap Gofer (if necessary), bootstrap ConfigurationOfMetacello (using old Gofer API), then load the latest 
	version of Metacello itself."

	Smalltalk at: #MetacelloProject ifAbsent: [
		Smalltalk at: #Gofer ifAbsent: [
			"Current version of Gofer from which to bootstrap - as of 1.0-beta.15"
			self bootstrapPackage: 'Gofer-lr.83' from: 'http://seaside.gemstone.com/ss/metacello' ].
		Smalltalk at: #Gofer ifPresent: [:goferClass | | gofer |
			gofer := goferClass new
				url: 'http://seaside.gemstone.com/ss/metacello';
				yourself.
			[ gofer addPackage: 'ConfigurationOfMetacello' ] on: Warning do: [:ex | ex resume ].
			gofer load ].
		"load 'default' group of Metacello"
		(Smalltalk at: #ConfigurationOfMetacello) perform: #load ]
]

{ #category : #'metacello tool support' }
MetacelloProjectRefTutorialConfig class >> isMetacelloConfig [
	"Answer true and the Metacello tools will operate on you"
	
	^true
]

{ #category : #accessing }
MetacelloProjectRefTutorialConfig class >> project [

	^self new project
]

{ #category : #baselines }
MetacelloProjectRefTutorialConfig >> baseline07: spec [ 
	<version: '0.7-baseline'>
	spec
		for: #common
		do: [spec blessing: #baseline.
			spec repository: 'http://www.example.com/Project'.
			spec 
				package: 'Project-Core' with: [
					spec requires: 'Example-Core' ];
				package: 'Project-Tests' with: [
					spec requires: #('Project-Core' 'Example-Tests' )];
				package: 'Example-Core' with: [
					spec repository: 'http://www.example.com/Example'];
				package: 'Example-Tests' with: [
					spec requires: 'Example-Core';
						 repository: 'http://www.example.com/Example'];
				package: 'Example-AddOn' with: [
					spec requires: 'Example-Core';
						 repository: 'http://www.example.com/Example']]
]

{ #category : #baselines }
MetacelloProjectRefTutorialConfig >> baseline10: spec [ 
	<version: '1.0-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.example.com/Project'.
		spec
			project: 'Example ALL' with: [
				spec
					className: 'MetacelloTutorialConfig';
					versionString: '1.0';
					loads: #('ALL' );
					file: 'MetacelloTutorialConfig';
					repository: 'http://seaside.gemstone.com/ss/metacello' ].
		spec 
			package: 'Project-Core' with: [ spec requires: 'Example ALL' ];
			package: 'Project-Tests' with: [ spec requires: 'Project-Core' ].].

]

{ #category : #baselines }
MetacelloProjectRefTutorialConfig >> baseline11: spec [ 
	<version: '1.1-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.example.com/Project'.

		spec
			project: 'Example Default' with: [
				spec
					className: 'MetacelloTutorialConfig';
					versionString: '1.1';
					loads: #('default' );
					file: 'Metacello-Tutorial';
					repository: 'http://seaside.gemstone.com/ss/metacello' ];
			project: 'Example Tests' with: [
				spec
					className: 'MetacelloTutorialConfig';
					versionString: '1.1';
					loads: #('Tests' );
					file: 'Metacello-Tutorial';
					repository: 'http://seaside.gemstone.com/ss/metacello' ].
		spec 
			package: 'Project-Core' with: [ spec requires: 'Example Default' ];
			package: 'Project-Tests' with: [ 
				spec requires: #('Project-Core' 'Example Tests' ).].].

]

{ #category : #baselines }
MetacelloProjectRefTutorialConfig >> baseline12: spec [ 
	<version: '1.2-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.example.com/Project'.
		spec
			project: 'Example Default' with: [
				spec
					className: 'MetacelloTutorialConfig';
					versionString: '1.2';
					loads: #('default' );
					file: 'Metacello-Tutorial';
					repository: 'http://seaside.gemstone.com/ss/metacello' ];
			project: 'Example Tests' 
				copyFrom: 'Example Default' 
				with: [ spec loads: #('Tests' ).].
		spec 
			package: 'Project-Core' with: [ spec requires: 'Example Default' ];
			package: 'Project-Tests' with: [ 
				spec requires: #('Project-Core' 'Example Tests' ).].].

]

{ #category : #baselines }
MetacelloProjectRefTutorialConfig >> baseline14: spec [ 
	<version: '1.4-baseline' imports: #('1.2-baseline' )>
	
	spec for: #common do: [
		spec description: 'Add groups and Project-Extra, extending 1.2-baseline'.
		spec 
			package: 'Project-Core' with: [ spec includes: 'Project-Extra' ];
			package: 'Project-Extra' with: [ spec requires: 'Project-Core' ].
		spec 
			group: 'default' with: #('Project-Core' 'Project-Extra' );
			group: 'Core' with: #('default' );
			group: 'Tests' with: #('Project-Tests' ).].

]

{ #category : #lessons }
MetacelloProjectRefTutorialConfig >> lesson07 [
"   [ see method: #baseline07: #version07: ]
NOTE: you should run through the lessons in MetacellTutorialConfig first.

In this configuration we are defining a project that utilizes the packages from the 
Example project (MetacelloTutorialConfig):  'Example-Core', 'Example-AddOn', 
'Example-Tests' and 2 packages specific to the project: 'Project-Core' and 
'Project-Tests':

  (MetacelloProjectRefTutorialConfig project version: '0.7') load.

"
]

{ #category : #lessons }
MetacelloProjectRefTutorialConfig >> lesson10 [
"   [ see method: #baseline10: #version10: ]
In lesson07 it was obvious that we copied the configuration information from 
MetacelloTutorialConfig and adapted it to our project.

There is a better way.

In #baseline10: we've created a project reference for the Example project.

The #className: specifies the name of the class that contains the project metadata. If 
the class is not present in the image, then we know that we need to load the 
configuration for the project.

The #file: and #repository: specifications give us the information needed to load the 
project metadata from a repository.

Finally, the #versionString: and #loads: tell us which version of the project to load 
and which packages to load from the project.

We've named the project reference 'Example ALL' and in the specification for the 
'Project-Core' package, we've specified that 'Example ALL' is required:

  (MetacelloProjectRefTutorialConfig project version: '1.0') load.

Note that the entire Example project is loaded before 'Project-Core'
"
]

{ #category : #lessons }
MetacelloProjectRefTutorialConfig >> lesson11 [
"   [ see method: #baseline11: #version11: ]
As is often the case, it is useful to separate the test package from the core packages 
for a project. 

In #baseline11: we've created two project references. The reference named 
'Example Default' loads the 'default' group and the reference named 'Example Tests' 
loads the 'Tests' group.

We then made 'Project-Core' require 'Example Default' and 'Project-Tests' requires 
'Project-Core' and 'Example Tests'.

Now it is possible to load just the core packages:

  (MetacelloProjectRefTutorialConfig project version: '1.1') load: 'Project-Core'.

or the whole enchilada including tests:

  (MetacelloProjectRefTutorialConfig project version: '1.1') load: 'Project-Tests'.

"
]

{ #category : #lessons }
MetacelloProjectRefTutorialConfig >> lesson12 [
"   [ see method: #baseline11: #baseline12: #version12: ]
In #baseline11: there is redundant information for each of the project references. 

In #baseline12: we use the #project:copyFrom:with: method to eliminate the need to 
specify the bulk of the project information twice. 

Evaluate and compare the results of the following expressions:
 
  (MetacelloProjectRefTutorialConfig project version: '1.1') load: 'Project-Core'.
  (MetacelloProjectRefTutorialConfig project version: '1.2') load: 'Project-Core'.

  (MetacelloProjectRefTutorialConfig project version: '1.1') load: 'Project-Tests'.
  (MetacelloProjectRefTutorialConfig project version: '1.2') load: 'Project-Tests'.

"
]

{ #category : #lessons }
MetacelloProjectRefTutorialConfig >> lesson13 [
"   [ see method: #version13: ]
In #version13: we are importing the '1.2-baseline', but changing the Example project 
version to 1.3, so project versions can be updated in the verson method jus like 
package versions.

Evaluate and compare the results of these expressions:

  (MetacelloProjectRefTutorialConfig project version: '1.2') load: 'Project-Core'.
  (MetacelloProjectRefTutorialConfig project version: '1.3') load: 'Project-Core'.

  (MetacelloProjectRefTutorialConfig project version: '1.2') load: 'Project-Tests'.
  (MetacelloProjectRefTutorialConfig project version: '1.3') load: 'Project-Tests'.

It is worth noting that in version 1.3 of the Example project, the platform-specific
'Example-Platform' was introduced and nothing special had to be done in the project 
reference to get the package included.
 
"
]

{ #category : #lessons }
MetacelloProjectRefTutorialConfig >> lesson14 [
"   [ see method: #baseline14: #version14: ]
In this lesson we'll cover the querying API for Metacello. The querying API is useful for analyzing the contents and structure of a version.

To start with we'll look at version '1.4' of the MetacelloProjectRefTutorialConfig. You can list the packages in the version:

  (MetacelloProjectRefTutorialConfig project version: '1.4') packages

The list project references:

  (MetacelloProjectRefTutorialConfig project version: '1.4') projects

And the groups:

  (MetacelloProjectRefTutorialConfig project version: '1.4') groups

You can access individual packages/project refs/groups using  the #packageNamed: method. Here you can access the package named: 'Project-Core':

  (MetacelloProjectRefTutorialConfig project version: '1.4') packageNamed: 'Project-Core'

The project reference named 'Example Default':

 (MetacelloProjectRefTutorialConfig project version: '1.4') packageNamed: 'Example Default'

The group named 'Core':

 (MetacelloProjectRefTutorialConfig project version: '1.4') packageNamed: 'Core'

Each of the attributes of the package can be accessed (#requires, #includes, #file, #repository, #preLoadDoIt, and #postLoadDoit). For example:

  ((MetacelloProjectRefTutorialConfig project version: '1.4') packageNamed: 'Project-Core') requires

Each of the attributes of the project can be accessed (#className, #versionString, #operator, #loads, #file, and #repository). For example:

  ((MetacelloProjectRefTutorialConfig project version: '1.4') packageNamed: 'Example Default') repository

Each of the attributes of the group can be accessed (#includes). For example:

  ((MetacelloProjectRefTutorialConfig project version: '1.4') packageNamed: 'default') includes

When looking at the 'Core' group, there is only one package listed:

  (MetacelloProjectRefTutorialConfig project version: '1.4') packageNamed: 'Core'

In the case of the 'Core' group, it is defined in terms of the 'default', which isn't very useful. When looking at the contents of groups you'd like to see the complete list of packages, without having to explicitly expanding each group you encounter. #packagesForSpecNamed: does just that:

  (MetacelloProjectRefTutorialConfig project version: '1.4') packagesForSpecNamed: 'Core'

If you were to load the 'Core' package:

  (MetacelloProjectRefTutorialConfig project version: '1.4') load: 'Core'

You end up seeing the packages from the Example project. If you want to get the list of packages that _would_ be loaded, you can use #allPackagesForSpecNamed:. For example:

  (MetacelloProjectRefTutorialConfig project version: '1.4') allPackagesForSpecNamed: 'Core'

If you use #allPackagesForSpecNamed: with a project refernce name, you'll see the packages that _would_ be loaded:

  (MetacelloProjectRefTutorialConfig project version: '1.4') load: 'Example Default'
  (MetacelloProjectRefTutorialConfig project version: '1.4') allPackagesForSpecNamed: 'Example Default'

You can also send #version to a project reference. This is useful if you want to explicitly walk the tree of projects:

  ((MetacelloProjectRefTutorialConfig project version: '1.4') packageNamed: 'Example Default') version

"
]

{ #category : #accessing }
MetacelloProjectRefTutorialConfig >> project [
	"NOTE: The MetacelloNullRecordingMCSpecLoader is being used to prevent packages
		from being loaded, see MetacelloConfigTemplate>>project for an example #project
		method that loads the package for real"

	^ project ifNil: [ | constructor loader |
		"Construct Metacello project"
		constructor := (Smalltalk at: #MetacelloVersionConstructor) on: self.
		project := constructor project.
		loader := MetacelloNullRecordingMCSpecLoader new.
		loader evalDoits: true.
		project loader: loader.
		project ]
]

{ #category : #versions }
MetacelloProjectRefTutorialConfig >> version07: spec [ 
	<version: '0.7' imports: #('0.7-baseline' )>
	
	spec for: #common do: [
		spec blessing: #release.
		
		spec 
			package: 'Project-Core' with: 'Project-Core-anon.1';
			package: 'Project-Tests' with: 'Project-Tests-anon.1';
			package: 'Example-Core' with: 'Example-Core-anon.12';
			package: 'Example-Tests' with: 'Example-Tests-anon.3';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1'.].

]

{ #category : #versions }
MetacelloProjectRefTutorialConfig >> version10: spec [ 
	<version: '1.0' imports: #('1.0-baseline' )>
	
	spec for: #common do: [
		spec blessing: #beta.
		
		spec 
			package: 'Project-Core' with: 'Project-Core-anon.1';
			package: 'Project-Tests' with: 'Project-Tests-anon.1'.].

]

{ #category : #versions }
MetacelloProjectRefTutorialConfig >> version11: spec [ 
	<version: '1.1' imports: #('1.1-baseline' )>
	
	spec for: #common do: [
		spec blessing: #beta.
		
		spec 
			package: 'Project-Core' with: 'Project-Core-anon.1';
			package: 'Project-Tests' with: 'Project-Tests-anon.1'.].

]

{ #category : #versions }
MetacelloProjectRefTutorialConfig >> version12: spec [ 
	<version: '1.2' imports: #('1.2-baseline' )>
	
	spec for: #common do: [
		spec blessing: #beta.
		spec 
			package: 'Project-Core' with: 'Project-Core-anon.1';
			package: 'Project-Tests' with: 'Project-Tests-anon.1'.].

]

{ #category : #versions }
MetacelloProjectRefTutorialConfig >> version13: spec [ 
	<version: '1.3' imports: #('1.2-baseline' )>
	
	spec for: #common do: [
		spec blessing: #beta.
		spec description: 'Import functionality from the example project, different packages'.

		spec 
			package: 'Project-Core' with: 'Project-Core-anon.1';
			package: 'Project-Tests' with: 'Project-Tests-anon.1';
			project: 'Example Default' with: '1.3';
			project: 'Example Tests' with: '1.3'].

]

{ #category : #versions }
MetacelloProjectRefTutorialConfig >> version14: spec [ 
	<version: '1.4' imports: #('1.4-baseline' )>
	
	spec for: #common do: [
		spec blessing: #beta.
		spec description: 'Add groups and Project-Extra'.

		spec
			project: 'Example Default' with: '1.3';
			project: 'Example Tests' with: '1.3'.
		spec 
			package: 'Project-Core' with: 'Project-Core-anon.2';
			package: 'Project-Tests' with: 'Project-Tests-anon.2';
			package: 'Project-Extra' with: 'Project-Extra-anon.1'.].

]
