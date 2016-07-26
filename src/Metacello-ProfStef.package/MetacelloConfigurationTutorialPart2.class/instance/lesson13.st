lessons
lesson13
	^Lesson
		title: 'Lesson 13'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloProjectRefTutorialConfig selector: #version13:.

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