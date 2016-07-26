lessons
lesson06
	^Lesson
		title: 'Lesson 6'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #version06:.

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