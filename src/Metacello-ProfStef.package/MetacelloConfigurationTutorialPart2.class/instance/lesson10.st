lessons
lesson10
	^Lesson
		title: 'Lesson 10'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloProjectRefTutorialConfig selector: #baseline10:.
Smalltalk tools browser fullOnClass: MetacelloProjectRefTutorialConfig selector: #version10:.

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
