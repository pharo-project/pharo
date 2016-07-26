lessons
lesson11Author
	^Lesson
		title: 'Lesson 11 (Author)'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #version11:.

"The author of a version can be defined:"

	(MetacelloTutorialConfig project version: ''1.1'') author.

"When using the OB-Metacello tools the author field is automatically updated to reflect the current
author as defined in the image."

ProfStef next.
'