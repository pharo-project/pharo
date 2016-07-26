lessons
lesson08
	^Lesson
		title: 'Lesson 8'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #version08:.

"In version 0.8 we''ve simply updated the package versions, which can be seen by comparing the 
results of loading version 0.7 and 0.8:"

  (MetacelloTutorialConfig project version: ''0.7'') load.
  (MetacelloTutorialConfig project version: ''0.8'') load.


ProfStef next.
'