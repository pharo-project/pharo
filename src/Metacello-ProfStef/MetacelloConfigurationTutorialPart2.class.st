Class {
	#name : #MetacelloConfigurationTutorialPart2,
	#superclass : #AbstractTutorial,
	#category : #'Metacello-ProfStef'
}

{ #category : #intro }
MetacelloConfigurationTutorialPart2 >> introductionText [

	^'Covers project reference specifications:
	1. Open a code browser on the MetacelloTutorialConfig class:"
		MetacelloTutorialConfig browse.
	"2. In the browser view the ''--all--'' category.
	3. Have fun!"'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart2 >> lesson07 [
	^Lesson
		title: 'Lesson 7'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloProjectRefTutorialConfig selector: #baseline07:.
Smalltalk tools browser openOnClass: MetacelloProjectRefTutorialConfig selector: #version07:.

"NOTE: you should run through the lessons in Part 1 first.

In this configuration we are defining a project that utilizes the packages from the 
Example project (MetacelloTutorialConfig):  ''Example-Core'', ''Example-AddOn'', 
''Example-Tests'' and 2 packages specific to the project: ''Project-Core'' and 
''Project-Tests'':"

  (MetacelloProjectRefTutorialConfig project version: ''0.7'') load.


ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart2 >> lesson10 [
	^Lesson
		title: 'Lesson 10'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloProjectRefTutorialConfig selector: #baseline10:.
Smalltalk tools browser openOnClass: MetacelloProjectRefTutorialConfig selector: #version10:.

"In lesson07 it was obvious that we copied the configuration information from 
MetacelloTutorialConfig and adapted it to our project.

There is a better way.

In #baseline10: we''ve created a project reference for the Example project.

The #className: specifies the name of the class that contains the project metadata. If 
the class is not present in the image, then we know that we need to load the 
configuration for the project.

The #file: and #repository: specifications give us the information needed to load the 
project metadata from a repository.

Finally, the #versionString: and #loads: tell us which version of the project to load 
and which packages to load from the project.

We''ve named the project reference ''Example ALL'' and in the specification for the 
''Project-Core'' package, we''ve specified that ''Example ALL'' is required:"

  (MetacelloProjectRefTutorialConfig project version: ''1.0'') load.

"Note that the entire Example project is loaded before ''Project-Core''"

ProfStef next.
'

]

{ #category : #lessons }
MetacelloConfigurationTutorialPart2 >> lesson11 [
	^Lesson
		title: 'Lesson 11'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloProjectRefTutorialConfig selector: #baseline11:.
Smalltalk tools browser openOnClass: MetacelloProjectRefTutorialConfig selector: #version11:.

"As is often the case, it is useful to separate the test package from the core packages 
for a project. 

In #baseline11: we''ve created two project references. The reference named 
''Example Default'' loads the ''default'' group and the reference named ''Example Tests'' 
loads the ''Tests'' group.

We then made ''Project-Core'' require ''Example Default'' and ''Project-Tests'' requires 
''Project-Core'' and ''Example Tests''.

Now it is possible to load just the core packages:"

  (MetacelloProjectRefTutorialConfig project version: ''1.1'') load: ''Project-Core''.

"or the whole enchilada including tests:"

  (MetacelloProjectRefTutorialConfig project version: ''1.1'') load: ''Project-Tests''.


ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart2 >> lesson12 [
	^Lesson
		title: 'Lesson 12'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloProjectRefTutorialConfig selector: #baseline11:.
Smalltalk tools browser openOnClass: MetacelloProjectRefTutorialConfig selector: #baseline12:.
Smalltalk tools browser openOnClass: MetacelloProjectRefTutorialConfig selector: #version12:.

"In #baseline11: there is redundant information for each of the project references. 

In #baseline12: we use the #project:copyFrom:with: method to eliminate the need to 
specify the bulk of the project information twice. 

Evaluate and compare the results of the following expressions:"
 
  (MetacelloProjectRefTutorialConfig project version: ''1.1'') load: ''Project-Core''.
  (MetacelloProjectRefTutorialConfig project version: ''1.2'') load: ''Project-Core''.

  (MetacelloProjectRefTutorialConfig project version: ''1.1'') load: ''Project-Tests''.
  (MetacelloProjectRefTutorialConfig project version: ''1.2'') load: ''Project-Tests''.


ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart2 >> lesson13 [
	^Lesson
		title: 'Lesson 13'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloProjectRefTutorialConfig selector: #version13:.

"In #version13: we are importing the ''1.2-baseline'', but changing the Example project 
version to 1.3, so project versions can be updated in the verson method jus like 
package versions.

Evaluate and compare the results of these expressions:"

  (MetacelloProjectRefTutorialConfig project version: ''1.2'') load: ''Project-Core''.
  (MetacelloProjectRefTutorialConfig project version: ''1.3'') load: ''Project-Core''.

  (MetacelloProjectRefTutorialConfig project version: ''1.2'') load: ''Project-Tests''.
  (MetacelloProjectRefTutorialConfig project version: ''1.3'') load: ''Project-Tests''.

"It is worth noting that in version 1.3 of the Example project, the platform-specific
''Example-Platform'' was introduced and nothing special had to be done in the project 
reference to get the package included."

ProfStef next.
'
]

{ #category : #lessons }
MetacelloConfigurationTutorialPart2 >> lesson14 [
	^Lesson
		title: 'Lesson 14'
		lesson: 
'
Smalltalk tools browser openOnClass: MetacelloProjectRefTutorialConfig selector: #baseline14:.
Smalltalk tools browser openOnClass: MetacelloProjectRefTutorialConfig selector: #version14:.

"In this lesson we''ll cover the querying API for Metacello. The querying API is useful for analyzing the contents and structure of a version.

To start with we''ll look at version ''1.4'' of the MetacelloProjectRefTutorialConfig. You can list the packages in the version:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') packages.

"The list project references:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') projects.

"And the groups:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') groups.

"You can access individual packages/project refs/groups using  the #packageNamed: method. Here you can access the package named: ''Project-Core'':"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Project-Core''.

"The project reference named ''Example Default'':"

 (MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Example Default''.

"The group named ''Core'':"

 (MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Core''.

"Each of the attributes of the package can be accessed (#requires, #includes, #file, #repository, #preLoadDoIt, and #postLoadDoit). For example:"

  ((MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Project-Core'') requires.

"Each of the attributes of the project can be accessed (#className, #versionString, #operator, #loads, #file, and #repository). For example:"

  ((MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Example Default'') repository.

"Each of the attributes of the group can be accessed (#includes). For example:"

  ((MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''default'') includes.

"When looking at the ''Core'' group, there is only one package listed:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Core''.

"In the case of the ''Core'' group, it is defined in terms of the ''default'', which isn''t very useful. When looking at the contents of groups you''d like to see the complete list of packages, without having to explicitly expanding each group you encounter. #packagesForSpecNamed: does just that:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') packagesForSpecNamed: ''Core''.

"If you were to load the ''Core'' package:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') load: ''Core''.

"You end up seeing the packages from the Example project. If you want to get the list of packages that _would_ be loaded, you can use #allPackagesForSpecNamed:. For example:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') allPackagesForSpecNamed: ''Core''.

"If you use #allPackagesForSpecNamed: with a project refernce name, you''ll see the packages that _would_ be loaded:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') load: ''Example Default''.
  (MetacelloProjectRefTutorialConfig project version: ''1.4'') allPackagesForSpecNamed: ''Example Default''.

"You can also send #version to a project reference. This is useful if you want to explicitly walk the tree of projects:"

  ((MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Example Default'') version.


ProfStef next.
'
]

{ #category : #tutorial }
MetacelloConfigurationTutorialPart2 >> tutorial [
	^ #(#lesson07 #lesson10 #lesson11 #lesson12 #lesson13 #lesson14)
]
