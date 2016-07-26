lessons
lesson03
	^Lesson
		title: 'Lesson 3'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #version03:.

"For version 0.3, we''ve updated the package version to ''Example-Core-anon.10'' and added an additional 
package ''Example-Tests-anon.3'', which can be confirmed by printing the following expression:"

  (MetacelloTutorialConfig project version: ''0.3'') spec

"As is often the case, the two packages share the same repository, so specifying a repository with
each package is redundant."

ProfStef next.
'