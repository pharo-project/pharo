lessons
lesson12
	^Lesson
		title: 'Lesson 12'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloProjectRefTutorialConfig selector: #baseline11:.
Smalltalk tools browser fullOnClass: MetacelloProjectRefTutorialConfig selector: #baseline12:.
Smalltalk tools browser fullOnClass: MetacelloProjectRefTutorialConfig selector: #version12:.

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