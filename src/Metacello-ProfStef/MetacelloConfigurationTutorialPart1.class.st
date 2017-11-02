Class {
	#name : #MetacelloConfigurationTutorialPart1,
	#superclass : #AbstractTutorial,
	#category : 'Metacello-ProfStef'
}

{ #category : #intro }
MetacelloConfigurationTutorialPart1 >> introductionText [

	^'Convers basic Metacello configuration specifications:
	1. Open a code browser on the MetacelloTutorialConfig class:"
		MetacelloTutorialConfig browse.
	"2. In the browser view the ''--all--'' category.
	3. Have fun!"'
	
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson01 [
	^Lesson
		title: 'Lesson 1'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version01:.

"Version 0.1 represents the simplest version specification possible. 

In the method #version01, version 0.1 is defined with a single package ''Example-Core-anon.8'' 
that is loaded from the repository ''http://www.example.com/Example''.

Looking at this method you will notice a couple of things. 

Immediately after the method selector you see the pragma definition:

  <version: ''0.1''>

The pragma indicates that the version created in this method should be associated with version 
''0.1'' of the Tutorial project.

Looking a little closer you see that the argument to the method, <spec>, is the only variable in
the method and it is used as the receiver to four different messages:

  - #for:do:
  - #package:with:
  - #file: 
  - #repository:

With the evaluation of each block expression, a new object is pushed on a stack and the messages
within the block are sent to the object on the top of the stack.

So the method should be read as:

  Create version ''0.1''. The #common code for version ''0.1'' (#for:do:) consists of a package named 
  ''Example-Core'' (#package:with:) whose file attribute is ''Example-Core-anon.8'' (#file:) and whose 
  repository attribute is ''http://www.example.com/Example'' (#repository:).

We can see the spec created for version 0.1 by printing the following expression:"

  (MetacelloTutorialConfig project version: ''0.1'') spec.

"Note that in creating version ''0.1'' the #common attribute is extracted out.

In addition to #common, there are pre-defined attributes for each of the platforms upon which
Metacello runs (#pharo, #squeak, #gemstone and #squeakCommon). 

#squeakCommon is used for both #pharo and #squeak."

ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson02 [
	^Lesson
		title: 'Lesson 2'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version02:.

"For version 0.2, we''ve simply updated the package version to ''Example-Core-anon.9'', which 
can be confirmed by printing the following expression:"

  (MetacelloTutorialConfig project version: ''0.2'') spec.

ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson03 [
	^Lesson
		title: 'Lesson 3'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version03:.

"For version 0.3, we''ve updated the package version to ''Example-Core-anon.10'' and added an additional 
package ''Example-Tests-anon.3'', which can be confirmed by printing the following expression:"

  (MetacelloTutorialConfig project version: ''0.3'') spec.

"As is often the case, the two packages share the same repository, so specifying a repository with
each package is redundant."

ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson04 [
	^Lesson
		title: 'Lesson 4'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version04:.

"The specification for version 0.4 is basically the same as version 0.3. Instead of listing a 
repository with each package we specify a project repository that applies to all packages.

Compare the printStrings for the specs for each version:"

  (MetacelloTutorialConfig project version: ''0.3'') spec.

  (MetacelloTutorialConfig project version: ''0.4'') spec.


ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson05 [
	^Lesson
		title: 'Lesson 5'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version05:.

"For version 0.5 we''ve added an additional package to the mix: ''Example-AddOn'':"

  (MetacelloTutorialConfig project version: ''0.5'') spec.

"Of course, the point of specifiying packages in Metacello is to be able to load versions. Here are
a couple examples of loading versions of the Tutorial. If you print the result of each expression, 
you will see the list of packages in load order (note that for the tutorial, we are using the 
MetacelloNullRecordingMCSpecLoader. This class records which packages are loaded and the order that they are loaded in among other things instead of actually loading the packages."

  (MetacelloTutorialConfig project version: ''0.1'') load.
  (MetacelloTutorialConfig project version: ''0.4'') load.
  (MetacelloTutorialConfig project version: ''0.5'') load.

"You will note that in each case, all of the packages associated with the version are loaded, which 
is the default. 

If you want to load a subset of the packages in a project, you may list the packages that you
are interested in as an argument to the #load: method:"

  (MetacelloTutorialConfig project version: ''0.5'') load: { ''Example-Tests''. ''Example-Core'' }.

"Note that the ordering of the packages is based on the order in which the packages are specified.

If you evaluate the following expression:"

  (MetacelloTutorialConfig project version: ''0.5'') load: { ''Example-Tests''. }.

"Only the package is ''Example-Tests''. By default the packages are ordered, but there are no implicit
dependencies."

ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson06 [
	^Lesson
		title: 'Lesson 6'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version06:.

"In version 0.6 we''ve added dependency information in the form of the #requires: directive.
Both ''Example-Tests'' and  ''Example-AddOn'' require ''Example-Core'' to be loaded before they are
loaded. Print the following expressions to see that the requires directives are being followed:"

  (MetacelloTutorialConfig project version: ''0.5'') load: { ''Example-Tests''. }.
  (MetacelloTutorialConfig project version: ''0.6'') load: { ''Example-Tests''. }.
  (MetacelloTutorialConfig project version: ''0.6'') load: ''Example-AddOn''.
  (MetacelloTutorialConfig project version: ''0.6'') load: { ''Example-AddOn''. ''Example-Tests''. }.

"With version 0.6 we are mixing structural information (required packages and repository) with the
dynamic file version info. It is expected that over time the file version info will change from
version to version while the structural information will remain relatively static."

ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson07 [
	^Lesson
		title: 'Lesson 7'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #baseline07:.
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version07:.

"For version 0.7, we are ceating a baseline version specification which is expected to be used
across several versions and the version specification which is restricted to the file versions.

In method #baseline07: the structure of version ''0.7-baseline'' is specified. The repository is 
listed, the packages are listed and the required packages are defined. We''ll cover the #blessing:
in a later lesson.

In method #version07: the file versions are specified. You will note that the pragma as an #imports: component that specifies the list of versions that this version (version ''0.7'') is based upon. In 
fact, if you print the spec for ''0.7-baseline'' and then print the spec for ''0.7'' you can see that 
''0.7'' is a composition of both versions:"

   (MetacelloTutorialConfig project version: ''0.7-baseline'') spec.
   (MetacelloTutorialConfig project version: ''0.7'') spec.

"Of course if you print the ''0.6'' spec and the ''0.7'' spec you can see that they specify exactly the
same information in a slightly different way:"

  (MetacelloTutorialConfig project version: ''0.6'') spec.
  (MetacelloTutorialConfig project version: ''0.7'') spec.

"and if you load each of the versions, you will see that they load the same packages, in the same 
order:"

  (MetacelloTutorialConfig project version: ''0.6'') load.
  (MetacelloTutorialConfig project version: ''0.7'') load.

"Finally, even though version ''0.7-baseline'' does not have explicit package versions, you may load
the version. When the ''real'' loader encounters a package name (without version information) it will
attempt to load the latest version of the package from the repository. With the
MetacelloNullRecordingMCSpecLoader the packages names are ''loaded'':"

  (MetacelloTutorialConfig project version: ''0.7-baseline'') load.

"Of course when a number of developers are working on a project it may be useful to load a
#baseline version so that you get the latest work from all of the project members. "

ProfStef next.

'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson08 [
	^Lesson
		title: 'Lesson 8'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version08:.

"In version 0.8 we''ve simply updated the package versions, which can be seen by comparing the 
results of loading version 0.7 and 0.8:"

  (MetacelloTutorialConfig project version: ''0.7'') load.
  (MetacelloTutorialConfig project version: ''0.8'') load.


ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson10 [
	^Lesson
		title: 'Lesson 10'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #baseline10:.
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version10:.

"In #baseline10: we''ve added two things: the ''Example-AddOnTests'' package and a specification for 
groups. 

The ''Example-AddOnTests'' package has been added to make the idea of needing to group packages a
little more appealing. The package requires ''Example-AddOn'' and ''Example-Tests''.

With two Test packages it would be convenient to be able to load all of the tests with a simple
expression like the following:"

  (MetacelloTutorialConfig project version: ''1.0'') load: { ''Tests''. }.

"instead of having to explicitly list all of the test projects like this:"

  (MetacelloTutorialConfig project version: ''1.0'') 
	load: { ''Example-Tests''. ''Example-AddOnTests''. }.

"This becomes especially useful if over time the project evolves to have more component and test 
packages.

The ''default'' group is special in that when a ''default'' group is defined, the #load method loads
the members of the ''default'' group instead of loading all of the packages:"

  (MetacelloTutorialConfig project version: ''1.0'') load.

"If you want to load all of the packages in a project, then the pseudo group ''ALL'' may be used as
follows:"

  (MetacelloTutorialConfig project version: ''1.0'') load: ''ALL''.


ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson11 [
	^Lesson
		title: 'Lesson 11'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version11:.

"In version 0.11 we''ve defined a couple of attributes that are expected to be used all of the time
in a version specification:

  #blessing:
  #description:
  #author:
  #timestamp:

The following lessons cover each of these attributes in more detail. "

ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson11Author [
	^Lesson
		title: 'Lesson 11 (Author)'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version11:.

"The author of a version can be defined:"

	(MetacelloTutorialConfig project version: ''1.1'') author.

"When using the OB-Metacello tools the author field is automatically updated to reflect the current
author as defined in the image."

ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson11Blessing [
	^Lesson
		title: 'Lesson 11 (Blessing)'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version11:.

"A version can be tagged with a blessing like #alpha, #beta, #release, #development or any 
other tag that you find useful. The blessing for version 1.1 is #development"

	(MetacelloTutorialConfig project version: ''1.1'') blessing.

"The default blessing is #release, so even though we didn''t specify a blessing for version 
0.5, the blessing is set:"

	(MetacelloTutorialConfig project version: ''0.5'') blessing.
	
"For version 1.1, it is important to explicitly set the blessing, because it imports version
''1.0-baseline'' whose blessing is #baseline:"

	(MetacelloTutorialConfig project version: ''1.0-baseline'') blessing.

"Blessings can be used as a filter. For example, you will notice that the result of the 
following expression is version 0.6, because #stableVersion answers the latest version whose 
blessing is _not_ #development, #broken, or #blessing: "

	MetacelloTutorialConfig project stableVersion.
	MetacelloTutorialConfig project stableVersion load.

"The blessing of version 1.1 is #development. To find the latest #development version you 
would execute this expression:"

	MetacelloTutorialConfig project latestVersion: #development.
	(MetacelloTutorialConfig project latestVersion: #development) load.

"You can get the very last version independent of blessing by executing this expression:"

	MetacelloTutorialConfig project bleedingEdge.
	MetacelloTutorialConfig project bleedingEdge load.
	
"In general, the #development blessing should be used for any version that is unstable. Once a 
version has stabilized, a different blessing should be applied.

The following expression will load the latest version of all of the packages for the latest 
#baseline version:"

	(MetacelloTutorialConfig project latestVersion: #baseline) load.

"Since the latest #baseline version should reflect the most up-to-date project structure, executing
the previous expression should load the absolute bleeding edge of the project. "

ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson11Descripton [
	^Lesson
		title: 'Lesson 11 (Description)'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version11:.

"A description can be defined for a version:"

	(MetacelloTutorialConfig project version: ''1.1'') description.
	

ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson11Timestamp [
	^Lesson
		title: 'Lesson 11 (Timestamp)'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version11:.

"The timestamp of a version can be defined:"

	(MetacelloTutorialConfig project version: ''1.1'') timestamp.

"When using the OB-Metacello tools the timestamp field is automatically updated to reflect the current
DateAndTime that the update was made.

The timestamp is a String"

ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson12DoIts [
	^Lesson
		title: 'Lesson 12 (DoIts)'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version12:.
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #preloadForCore.
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #postloadForCore:package:.

"Occassionally, you find that you need to perform an expression either before a package is loaded, or
after a package is loaded. To do that in Metacello, you can define a preLoadDoIt selector and a 
postLoadDoIt selector:"

	(MetacelloTutorialConfig project version: ''1.2'') spec.

"If you open a Transcript and execute the following expression, you will see that the pre load and
post load methods were executed:"

	(MetacelloTutorialConfig project version: ''1.2'') load.

"The pre/post load methods may take 0, 1 or 2 args. The loader is the first optional argument and the 
loaded packageSpec is the second optional argument."

ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart1 >> lesson13 [
	^Lesson
		title: 'Lesson 12 (DoIts)'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #baseline13:.
Smalltalk tools browser openOnClass: MetacelloTutorialConfig selector: #version13:.

"For version 1.3 we are adding a platform specific package ''Example-Platform''. ''Example-Platform''
requires ''Example-Core''. On GemStone, Pharo and Squeak, a branch of the ''Example-Platform'' package 
will be loaded: ''Example-Platform.gemstone'', ''Example-Platform.pharo'', ''Example-Platform.squeak'' 
respectively will be loaded. 

Consequently we''ve updated the baselines with #baseline13: to reflect the structural changes and
#version13: reflects the package versions.

The platform-specific versions and branches are defined in the #for:do: block for the corresponding
platforms: #gemstone, #pharo, #squeak (in both methods)

The result of the following expression will depend on the platform upon which you are running:"

	(MetacelloTutorialConfig project version: ''1.3'') load.

"Note that when you execute the following expresson to load ''Example-Core'' that the correct
''Example-Platform'' is loaded as well:"

	(MetacelloTutorialConfig project version: ''1.3'') load: ''Example-Core''.

"If you look at the specification for ''Example-Core'' (in #baseline13:) you will note that 
''Example-Core'' #includes: ''Example-Platform''. The #includes: directive means that the package 
''Example-Platform'' should be loaded whenever the ''Example-Core'' package is loaded.

Also note when you evaluate the following expression that the ''Example-Platform'' package is loaded
before ''Example-Tests'' as if ''Example-Tests'' #requires: ''Example-Platform'':"

	(MetacelloTutorialConfig project version: ''1.3'') load: ''Example-Tests''.

"When you use the #includes: directive, you are not only specifying that the listed packages should
be loaded when the parent package is loaded, but that the #included: packages should be loaded
_before_ any packages that require the parent package."

ProfStef next.
'
]

{ #category : #tutorial }
MetacelloConfigurationTutorialPart1 >> tutorial [
	^ #(#lesson01 #lesson02 #lesson03 #lesson04 #lesson05 #lesson06 #lesson07 #lesson08 #lesson10 #lesson11 #lesson11Author #lesson11Blessing #lesson11Descripton #lesson11Timestamp #lesson12DoIts #lesson13)
]
