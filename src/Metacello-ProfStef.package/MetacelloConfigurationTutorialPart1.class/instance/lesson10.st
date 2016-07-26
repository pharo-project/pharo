lessons
lesson10
	^Lesson
		title: 'Lesson 10'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #baseline10:.
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #version10:.

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