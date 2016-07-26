lessons
lesson11
	^Lesson
		title: 'Lesson 11'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloProjectRefTutorialConfig selector: #baseline11:.
Smalltalk tools browser fullOnClass: MetacelloProjectRefTutorialConfig selector: #version11:.

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