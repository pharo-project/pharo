Class {
	#name : #MetacelloTutorialConfig,
	#superclass : #Object,
	#instVars : [
		'project'
	],
	#category : #'Metacello-Tutorial'
}

{ #category : #private }
MetacelloTutorialConfig class >> bootstrapPackage: aString from: aPath [ 
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
MetacelloTutorialConfig class >> ensureMetacello [
	"Bootstrap Gofer (if necessary), bootstrap ConfigurationOfMetacello (using old Gofer API), then load the latest 
	version of Metacello itself."

	| env |
	env := self environment.
	env
		at: #MetacelloProject
		ifAbsent: [ env
				at: #Gofer
				ifAbsent:
					[ "Current version of Gofer from which to bootstrap - as of 1.0-beta.15"
					self
						bootstrapPackage: 'Gofer-lr.83'
						from: 'http://seaside.gemstone.com/ss/metacello' ].
			env
				at: #Gofer
				ifPresent: [ :goferClass | 
					| gofer |
					gofer := goferClass new
						url: 'http://seaside.gemstone.com/ss/metacello';
						yourself.
					[ gofer addPackage: 'ConfigurationOfMetacello' ]
						on: Warning
						do: [ :ex | ex resume ].
					gofer load ].
			"load 'default' group of Metacello"
			(env at: #ConfigurationOfMetacello) perform: #load ]
]

{ #category : #'metacello tool support' }
MetacelloTutorialConfig class >> isMetacelloConfig [
	"Answer true and the Metacello tools will operate on you"
	
	^true
]

{ #category : #accessing }
MetacelloTutorialConfig class >> project [

	^self new project
]

{ #category : #baselines }
MetacelloTutorialConfig >> baseline07: spec [ 
	<version: '0.7-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.example.com/Example'.
		
		spec 
			package: 'Example-Core';
			package: 'Example-Tests' with: [ spec requires: 'Example-Core' ];
			package: 'Example-AddOn' with: [ spec requires: 'Example-Core' ] ].

]

{ #category : #baselines }
MetacelloTutorialConfig >> baseline10: spec [ 
	<version: '1.0-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.example.com/Example'.
		
		spec 
			package: 'Example-Core';
			package: 'Example-Tests' with: [ spec requires: 'Example-Core' ];
			package: 'Example-AddOn' with: [ spec requires: 'Example-Core' ];
			package: 'Example-AddOnTests' with: [ 
				spec requires: #('Example-AddOn' 'Example-Tests' ) ].
		spec 
			group: 'default' with: #('Example-Core' 'Example-AddOn' );
			group: 'Tests' with: #('Example-Tests' 'Example-AddOnTests' ) ].

]

{ #category : #baselines }
MetacelloTutorialConfig >> baseline13: spec [ 
	<version: '1.3-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.example.com/Example'.
		
		spec 
			package: 'Example-Core' with: [
				spec includes: #('Example-Platform' ) ];
			package: 'Example-Tests' with: [ spec requires: 'Example-Core' ];
			package: 'Example-AddOn' with: [ spec requires: 'Example-Core' ];
			package: 'Example-Platform' with: [ spec requires: 'Example-Core' ];
			package: 'Example-AddOnTests' with: [ 
				spec requires: #('Example-AddOn' 'Example-Tests' ) ].
		spec 
			group: 'default' with: #('Example-Core' 'Example-AddOn' );
			group: 'Tests' with: #('Example-Tests' 'Example-AddOnTests' ) ].
	spec for: #gemstone do: [
		spec package: 'Example-Platform' with: 'Example-Platform.gemstone'.].
	spec for: #pharo do: [
		spec package: 'Example-Platform' with: 'Example-Platform.pharo'.].
	spec for: #squeak do: [
		spec package: 'Example-Platform' with: 'Example-Platform.squeak'.].
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson01 [
" [ see metod: #version01: ]
Version 0.1 represents the simplest version specification possible. 

In the method #version01, version 0.1 is defined with a single package 'Example-Core-anon.8' 
that is loaded from the repository 'http://www.example.com/Example'.

Looking at this method you will notice a couple of things. 

Immediately after the method selector you see the pragma definition:

  <version: '0.1'>

The pragma indicates that the version created in this method should be associated with version 
'0.1' of the Tutorial project.

Looking a little closer you see that the argument to the method, <spec>, is the only variable in
the method and it is used as the receiver to four different messages:

  - #for:do:
  - #package:with:
  - #file: 
  - #repository:

With the evaluation of each block expression, a new object is pushed on a stack and the messages
within the block are sent to the object on the top of the stack.

So the method should be read as:

  Create version '0.1'. The #common code for version '0.1' (#for:do:) consists of a package named 
  'Example-Core' (#package:with:) whose file attribute is 'Example-Core-anon.8' (#file:) and whose 
  repository attribute is 'http://www.example.com/Example' (#repository:).

We can see the spec created for version 0.1 by printing the following expression:

  (MetacelloTutorialConfig project version: '0.1') spec

Note that in creating version '0.1' the #common attribute is extracted out.

In addition to #common, there are pre-defined attributes for each of the platforms upon which
Metacello runs (#pharo, #squeak, #gemstone and #squeakCommon). 

#squeakCommon is used for both #pharo and #squeak.
"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson02 [
"  [ see method: #version02: ]
For version 0.2, we've simply updated the package version to 'Example-Core-anon.9', which 
can be confirmed by printing the following expression:

  (MetacelloTutorialConfig project version: '0.2') spec

"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson03 [
"   [ see method: #version03: ]
For version 0.3, we've updated the package version to 'Example-Core-anon.10' and added an additional 
package 'Example-Tests-anon.3', which can be confirmed by printing the following expression:

  (MetacelloTutorialConfig project version: '0.3') spec

As is often the case, the two packages share the same repository, so specifying a repository with
each package is redundant.

"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson04 [
"   [ see method: #version04: ]
The specification for version 0.4 is basically the same as version 0.3. Instead of listing a 
repository with each package we specify a project repository that applies to all packages.

Compare the printStrings for the specs for each version:

  (MetacelloTutorialConfig project version: '0.3') spec

  (MetacelloTutorialConfig project version: '0.4') spec
"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson05 [
"   [ see method: #version05: ]
For version 0.5 we've added an additional package to the mix: 'Example-AddOn':

  (MetacelloTutorialConfig project version: '0.5') spec

Of course, the point of specifiying packages in Metacello is to be able to load versions. Here are
a couple examples of loading versions of the Tutorial. If you print the result of each expression, 
you will see the list of packages in load order (note that for the tutorial, we are using the 
MetacelloNullRecordingMCSpecLoader. This class records which packages are loaded and the order that they are loaded in among other things instead of actually loading the packages.

  (MetacelloTutorialConfig project version: '0.1') load.
  (MetacelloTutorialConfig project version: '0.4') load.
  (MetacelloTutorialConfig project version: '0.5') load.

You will note that in each case, all of the packages associated with the version are loaded, which 
is the default. 

If you want to load a subset of the packages in a project, you may list the packages that you
are interested in as an argument to the #load: method:

  (MetacelloTutorialConfig project version: '0.5') load: { 'Example-Tests'. 'Example-Core' }.

Note that the ordering of the packages is based on the order in which the packages are specified.

If you evaluate the following expression:

  (MetacelloTutorialConfig project version: '0.5') load: { 'Example-Tests'. }.

Only the package is 'Example-Tests'. By default the packages are ordered, but there are no implicit
dependencies.
"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson06 [
"   [ see method: #version06: ]
In version 0.6 we've added dependency information in the form of the #requires: directive.
Both 'Example-Tests' and  'Example-AddOn' require 'Example-Core' to be loaded before they are
loaded. Print the following expressions to see that the requires directives are being followed:

  (MetacelloTutorialConfig project version: '0.5') load: { 'Example-Tests'. }.
  (MetacelloTutorialConfig project version: '0.6') load: { 'Example-Tests'. }.
  (MetacelloTutorialConfig project version: '0.6') load: 'Example-AddOn'.
  (MetacelloTutorialConfig project version: '0.6') load: { 'Example-AddOn'. 'Example-Tests'. }.

With version 0.6 we are mixing structural information (required packages and repository) with the
dynamic file version info. It is expected that over time the file version info will change from
version to version while the structural information will remain relatively static.

"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson07 [
"   [ see method: #baseline07: and #version07: ]
For version 0.7, we are ceating a baseline version specification which is expected to be used
across several versions and the version specification which is restricted to the file versions.

In method #baseline07: the structure of version '0.7-baseline' is specified. The repository is 
listed, the packages are listed and the required packages are defined. We'll cover the #blessing:
in a later lesson.

In method #version07: the file versions are specified. You will note that the pragma as an #imports: component that specifies the list of versions that this version (version '0.7') is based upon. In 
fact, if you print the spec for '0.7-baseline' and then print the spec for '0.7' you can see that 
'0.7' is a composition of both versions:

   (MetacelloTutorialConfig project version: '0.7-baseline') spec.
   (MetacelloTutorialConfig project version: '0.7') spec.

Of course if you print the '0.6' spec and the '0.7' spec you can see that they specify exactly the
same information in a slightly different way:

  (MetacelloTutorialConfig project version: '0.6') spec.
  (MetacelloTutorialConfig project version: '0.7') spec.

and if you load each of the versions, you will see that they load the same packages, in the same 
order:

  (MetacelloTutorialConfig project version: '0.6') load.
  (MetacelloTutorialConfig project version: '0.7') load.

Finally, even though version '0.7-baseline' does not have explicit package versions, you may load
the version. When the 'real' loader encounters a package name (without version information) it will
attempt to load the latest version of the package from the repository. With the
MetacelloNullRecordingMCSpecLoader the packages names are 'loaded':

  (MetacelloTutorialConfig project version: '0.7-baseline') load.

Of course when a number of developers are working on a project it may be useful to load a
#baseline version so that you get the latest work from all of the project members. 

"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson08 [
"   [ see method: #version08: ]
In version 0.8 we've simply updated the package versions, which can be seen by comparing the 
results of loading version 0.7 and 0.8:

  (MetacelloTutorialConfig project version: '0.7') load.
  (MetacelloTutorialConfig project version: '0.8') load.

"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson10 [
"   [ see method: #baseline10: #version10: ]
In #baseline10: we've added two things: the 'Example-AddOnTests' package and a specification for 
groups. 

The 'Example-AddOnTests' package has been added to make the idea of needing to group packages a
little more appealing. The package requires 'Example-AddOn' and 'Example-Tests'.

With two Test packages it would be convenient to be able to load all of the tests with a simple
expression like the following:

  (MetacelloTutorialConfig project version: '1.0') load: { 'Tests'. }.

instead of having to explicitly list all of the test projects like this:

  (MetacelloTutorialConfig project version: '1.0') 
	load: { 'Example-Tests'. 'Example-AddOnTests'. }.

This becomes especially useful if over time the project evolves to have more component and test 
packages.

The 'default' group is special in that when a 'default' group is defined, the #load method loads
the members of the 'default' group instead of loading all of the packages:

  (MetacelloTutorialConfig project version: '1.0') load.

If you want to load all of the packages in a project, then the pseudo group 'ALL' may be used as
follows:

  (MetacelloTutorialConfig project version: '1.0') load: 'ALL'.

"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson11 [
"   [ see method: #version11: ]
In version 0.11 we've defined a couple of attributes that are expected to be used all of the time
in a version specification:

  #blessing:
  #description:
  #author:
  #timestamp:

The following lessons cover each of these attributes in more detail. 
"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson11Author [
"  [ see method: #version11: ]
The author of a version can be defined:

	(MetacelloTutorialConfig project version: '1.1') author.

When using the OB-Metacello tools the author field is automatically updated to reflect the current
author as defined in the image.

"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson11Blessing [
"   [ see method: #version11: ]
A version can be tagged with a blessing like #alpha, #beta, #release, #development or any 
other tag that you find useful. The blessing for version 1.1 is #development

	(MetacelloTutorialConfig project version: '1.1') blessing.

The default blessing is #release, so even though we didn't specify a blessing for version 
0.5, the blessing is set:

	(MetacelloTutorialConfig project version: '0.5') blessing.
	
For version 1.1, it is important to explicitly set the blessing, because it imports version
'1.0-baseline' whose blessing is #baseline:

	(MetacelloTutorialConfig project version: '1.0-baseline') blessing.

Blessings can be used as a filter. For example, you will notice that the result of the 
following expression is version 0.6, because #stableVersion answers the latest version whose 
blessing is _not_ #development, #broken, or #blessing: 

	MetacelloTutorialConfig project stableVersion.
	MetacelloTutorialConfig project stableVersion load.

The blessing of version 1.1 is #development. To find the latest #development version you 
would execute this expression:

	MetacelloTutorialConfig project latestVersion: #development.
	(MetacelloTutorialConfig project latestVersion: #development) load.

You can get the very last version independent of blessing by executing this expression:

	MetacelloTutorialConfig project bleedingEdge.
	MetacelloTutorialConfig project bleedingEdge load.
	
In general, the #development blessing should be used for any version that is unstable. Once a 
version has stabilized, a different blessing should be applied.

The following expression will load the latest version of all of the packages for the latest 
#baseline version:

	(MetacelloTutorialConfig project latestVersion: #baseline) load.

Since the latest #baseline version should reflect the most up-to-date project structure, executing
the previous expression should load the absolute bleeding edge of the project. 
"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson11Descripton [
"  [ see method: #version11: ]
A description can be defined for a version:

	(MetacelloTutorialConfig project version: '1.1') description.

"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson11Timestamp [
"  [ see method: #version11: ]
The timestamp of a version can be defined:

	(MetacelloTutorialConfig project version: '1.1') timestamp.

When using the OB-Metacello tools the timestamp field is automatically updated to reflect the current
DateAndTime that the update was made.

The timestamp is a String
"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson12DoIts [
"  [ see methods: #version12: #preloadForCore #postloadForCore:package: ]
Occassionally, you find that you need to perform an expression either before a package is loaded, or
after a package is loaded. To do that in Metacello, you can define a preLoadDoIt selector and a 
postLoadDoIt selector:

	(MetacelloTutorialConfig project version: '1.2') spec.

If you open a Transcript and execute the following expression, you will see that the pre load and
post load methods were executed:

	(MetacelloTutorialConfig project version: '1.2') load.

The pre/post load methods may take 0, 1 or 2 args. The loader is the first optional argument and the 
loaded packageSpec is the second optional argument.
"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson13 [
"  [ see methods: #baseline13: #version13: ]
For version 1.3 we are adding a platform specific package 'Example-Platform'. 'Example-Platform'
requires 'Example-Core'. On GemStone, Pharo and Squeak, a branch of the 'Example-Platform' package 
will be loaded: 'Example-Platform.gemstone', 'Example-Platform.pharo', 'Example-Platform.squeak' 
respectively will be loaded. 

Consequently we've updated the baselines with #baseline13: to reflect the structural changes and
#version13: reflects the package versions.

The platform-specific versions and branches are defined in the #for:do: block for the corresponding
platforms: #gemstone, #pharo, #squeak (in both methods)

The result of the following expression will depend on the platform upon which you are running:

	(MetacelloTutorialConfig project version: '1.3') load.

Note that when you execute the following expresson to load 'Example-Core' that the correct
'Example-Platform' is loaded as well:

	(MetacelloTutorialConfig project version: '1.3') load: 'Example-Core'.

If you look at the specification for 'Example-Core' (in #baseline13:) you will note that 
'Example-Core' #includes: 'Example-Platform'. The #includes: directive means that the package 
'Example-Platform' should be loaded whenever the 'Example-Core' package is loaded.

Also note when you evaluate the following expression that the 'Example-Platform' package is loaded
before 'Example-Tests' as if 'Example-Tests' #requires: 'Example-Platform':

	(MetacelloTutorialConfig project version: '1.3') load: 'Example-Tests'.

When you use the #includes: directive, you are not only specifying that the listed packages should
be loaded when the parent package is loaded, but that the #included: packages should be loaded
_before_ any packages that require the parent package.


"
]

{ #category : #lessons }
MetacelloTutorialConfig >> lesson14 [
"
STILL UNDER CONSTRUCTION: Open your browsers on the class MetacelloProjectRefTutorial to continue the tutorial.
"
]

{ #category : #doits }
MetacelloTutorialConfig >> postloadForCore: loader package: packageSpec [

	MetacelloNotification signal: ('#postloadForCore executed, Loader: ', loader printString, 
			' spec: ', packageSpec printString).
]

{ #category : #doits }
MetacelloTutorialConfig >> preloadForCore [

	MetacelloNotification signal: '#preloadForCore executed'.
]

{ #category : #accessing }
MetacelloTutorialConfig >> project [
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
MetacelloTutorialConfig >> version01: spec [ 
	<version: '0.1'>
	
	spec for: #common do: [
		spec 
			package: 'Example-Core' with: [
				spec 
					file: 'Example-Core-anon.8';
					repository: 'http://www.example.com/Example' ] ].

]

{ #category : #versions }
MetacelloTutorialConfig >> version02: spec [ 
	<version: '0.2'>
	
	spec for: #common do: [
		spec 
			package: 'Example-Core' with: [
				spec 
					file: 'Example-Core-anon.9';
					repository: 'http://www.example.com/Example' ] ].

]

{ #category : #versions }
MetacelloTutorialConfig >> version03: spec [ 
	<version: '0.3'>
	
	spec for: #common do: [
		spec 
			package: 'Example-Core' with: [
				spec 
					file: 'Example-Core-anon.10';
					repository: 'http://www.example.com/Example' ];
			package: 'Example-Tests' with: [
				spec 
					file: 'Example-Tests-anon.3';
					repository: 'http://www.example.com/Example' ]].

]

{ #category : #versions }
MetacelloTutorialConfig >> version04: spec [ 
	<version: '0.4'>
	
	spec for: #common do: [
		spec repository: 'http://www.example.com/Example'.
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.10';
			package: 'Example-Tests' with: 'Example-Tests-anon.3' ].

]

{ #category : #versions }
MetacelloTutorialConfig >> version05: spec [ 
	<version: '0.5'>
	
	spec for: #common do: [
		spec repository: 'http://www.example.com/Example'.
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.11';
			package: 'Example-Tests' with: 'Example-Tests-anon.3';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1' ].

]

{ #category : #versions }
MetacelloTutorialConfig >> version06: spec [ 
	<version: '0.6'>
	
	spec for: #common do: [
		spec repository: 'http://www.example.com/Example'.
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.12';
			package: 'Example-Tests' with: [
				spec 
					file: 'Example-Tests-anon.3';
					requires: 'Example-Core' ];
			package: 'Example-AddOn' with: [
				spec 
					file: 'Example-AddOn-anon.1';
					requires: 'Example-Core' ]].

]

{ #category : #versions }
MetacelloTutorialConfig >> version07: spec [ 
	<version: '0.7' imports: #('0.7-baseline')>
	
	spec for: #common do: [
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.12';
			package: 'Example-Tests' with: 'Example-Tests-anon.3';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1' ].

]

{ #category : #versions }
MetacelloTutorialConfig >> version08: spec [ 
	<version: '0.8' imports: #('0.7-baseline')>
	
	spec for: #common do: [
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.15';
			package: 'Example-Tests' with: 'Example-Tests-anon.6';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.2' ].

]

{ #category : #versions }
MetacelloTutorialConfig >> version10: spec [ 
	<version: '1.0' imports: #('1.0-baseline')>
	
	spec for: #common do: [
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.14';
			package: 'Example-Tests' with: 'Example-Tests-anon.3';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1';
			package: 'Example-AddOnTests' with: 'Example-AddOnTests-anon.1' ].

]

{ #category : #versions }
MetacelloTutorialConfig >> version11: spec [ 
	<version: '1.1' imports: #('1.0-baseline')>
	
	spec for: #common do: [
		spec blessing: #development.
		spec description: 'Example of a complete version specification'.
		spec author: 'dkh'.
		spec timestamp: '10/12/2009 09:26'.
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.15';
			package: 'Example-Tests' with: 'Example-Tests-anon.4';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1';
			package: 'Example-AddOnTests' with: 'Example-AddOnTests-anon.1' ].

]

{ #category : #versions }
MetacelloTutorialConfig >> version12: spec [ 
	<version: '1.2' imports: #('1.0-baseline')>
	
	spec for: #common do: [
		spec blessing: #development.
		spec description: 'Add in doits for Example-Core'.
		spec author: 'dkh'.
		spec timestamp: '10/12/2009 09:26'.
		spec 
			package: 'Example-Core' with: [
				spec 
					file: 'Example-Core-anon.16';
					preLoadDoIt: #preloadForCore;
					postLoadDoIt: #postloadForCore:package: ];
			package: 'Example-Tests' with: 'Example-Tests-anon.5';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1';
			package: 'Example-AddOnTests' with: 'Example-AddOnTests-anon.1' ].

]

{ #category : #versions }
MetacelloTutorialConfig >> version13: spec [ 
	<version: '1.3' imports: #('1.3-baseline')>
	
	spec for: #common do: [
		spec blessing: #development.
		spec description: 'Add in doits for Example-Core'.
		spec author: 'dkh'.
		spec timestamp: '10/13/2009 14:27'.
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.17';
			package: 'Example-Tests' with: 'Example-Tests-anon.6';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1';
			package: 'Example-AddOnTests' with: 'Example-AddOnTests-anon.1' ].
	spec for: #gemstone do: [
		spec package: 'Example-Platform' with: 'Example-Platform.gemstone-dkh.4'.].
	spec for: #pharo do: [
		spec package: 'Example-Platform' with: 'Example-Platform.pharo-dkh.7'.].
	spec for: #squeak do: [
		spec package: 'Example-Platform' with: 'Example-Platform.squeak-dkh.3'.].
]
