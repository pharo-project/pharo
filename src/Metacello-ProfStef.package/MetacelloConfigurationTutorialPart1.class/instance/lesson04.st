lessons
lesson04
	^Lesson
		title: 'Lesson 4'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #version04:.

"The specification for version 0.4 is basically the same as version 0.3. Instead of listing a 
repository with each package we specify a project repository that applies to all packages.

Compare the printStrings for the specs for each version:"

  (MetacelloTutorialConfig project version: ''0.3'') spec.

  (MetacelloTutorialConfig project version: ''0.4'') spec.


ProfStef next.
'